/* The explicit-control evaluator of section 5.4 written in C.
 *
 * The registers are C variables and the controller is one function whose
 * labels are C labels. The continue register holds a Label, and
 * go_to_continue jumps to the C label it names. Procedure application and
 * sequences do not save anything before their last step, so the evaluator
 * runs iterative processes in constant space. */

#include "eval.h"

#include "environment.h"
#include "error.h"
#include "memory.h"
#include "primitives.h"

typedef enum {
    DONE,
    EV_APPL_DID_OPERATOR,
    EV_APPL_ACCUMULATE_ARG,
    EV_APPL_ACCUM_LAST_ARG,
    EV_SEQUENCE_CONTINUE,
    EV_IF_DECIDE,
    EV_COND_DECIDE,
    EV_ASSIGNMENT_1,
    EV_DEFINITION_1
} Label;

static struct {
    Value exp, env, val, cont, proc, argl, unev;
} reg;

static struct {
    Value quote, set, define, if_, lambda, begin, cond, let, else_, ok;
} symbol;

void init_evaluator(void)
{
    add_root(&reg.exp);
    add_root(&reg.env);
    add_root(&reg.val);
    add_root(&reg.cont);
    add_root(&reg.proc);
    add_root(&reg.argl);
    add_root(&reg.unev);

    symbol.quote = intern("quote");
    symbol.set = intern("set!");
    symbol.define = intern("define");
    symbol.if_ = intern("if");
    symbol.lambda = intern("lambda");
    symbol.begin = intern("begin");
    symbol.cond = intern("cond");
    symbol.let = intern("let");
    symbol.else_ = intern("else");
    symbol.ok = intern("ok");
}

void reset_evaluator(void)
{
    reg.exp = reg.env = reg.val = reg.proc = reg.argl = reg.unev = EMPTY_LIST;
    reg.cont = make_label(DONE);
    reset_stack();
}

/*** Syntax ***/

static bool is_tagged_list(Value exp, Value tag)
{
    return is_pair(exp) && is_eq(car(exp), tag);
}

static bool is_self_evaluating(Value exp)
{
    return is_number(exp) || is_string(exp) || is_boolean(exp);
}

static Value make_lambda(Value parameters, Value body)
{
    return cons(symbol.lambda, cons(parameters, body));
}

static Value lambda_parameters(Value exp) { return cadr(exp); }
static Value lambda_body(Value exp) { return cddr(exp); }

static Value definition_variable(Value exp)
{
    return is_symbol(cadr(exp)) ? cadr(exp) : car(cadr(exp));
}

static Value definition_value(Value exp)
{
    if (is_symbol(cadr(exp)))
        return caddr(exp);
    return make_lambda(cdr(cadr(exp)), cddr(exp));
}

static bool has_alternative(Value exp) { return !is_null(cdddr(exp)); }

static Value reverse_in_place(Value list)
{
    Value reversed = EMPTY_LIST;
    while (is_pair(list)) {
        Value rest = cdr(list);
        set_cdr(list, reversed);
        reversed = list;
        list = rest;
    }
    return reversed;
}

/* (let ((v e) ...) body ...) is ((lambda (v ...) body ...) e ...) */
static Value let_to_combination(Value exp)
{
    Value bindings = cadr(exp);
    Value variables = EMPTY_LIST, operands = EMPTY_LIST;
    protect(&exp);
    protect(&bindings);
    protect(&variables);
    protect(&operands);
    for (; is_pair(bindings); bindings = cdr(bindings)) {
        variables = cons(caar(bindings), variables);
        operands = cons(cadr(car(bindings)), operands);
    }
    Value lambda = make_lambda(reverse_in_place(variables), cddr(exp));
    Value combination = cons(lambda, reverse_in_place(operands));
    unprotect(4);
    return combination;
}

/* A compound procedure is a cell holding its lambda expression and the
 * environment it was made in. */
static Value make_procedure(Value lambda, Value env)
{
    return make_cell(TYPE_PROCEDURE, lambda, env);
}

static Value procedure_parameters(Value p)
{
    return lambda_parameters(cell_car(p));
}

static Value procedure_body(Value p) { return lambda_body(cell_car(p)); }
static Value procedure_environment(Value p) { return cell_cdr(p); }

/*** The controller ***/

static void execute(void)
{
eval_dispatch:
    if (is_self_evaluating(reg.exp)) {
        reg.val = reg.exp;
        goto go_to_continue;
    }
    if (is_symbol(reg.exp)) {
        reg.val = lookup_variable_value(reg.exp, reg.env);
        goto go_to_continue;
    }
    if (is_tagged_list(reg.exp, symbol.quote)) {
        reg.val = cadr(reg.exp);
        goto go_to_continue;
    }
    if (is_tagged_list(reg.exp, symbol.set))
        goto ev_assignment;
    if (is_tagged_list(reg.exp, symbol.define))
        goto ev_definition;
    if (is_tagged_list(reg.exp, symbol.if_))
        goto ev_if;
    if (is_tagged_list(reg.exp, symbol.lambda)) {
        reg.val = make_procedure(reg.exp, reg.env);
        goto go_to_continue;
    }
    if (is_tagged_list(reg.exp, symbol.begin)) {
        reg.unev = cdr(reg.exp);
        save(reg.cont);
        goto ev_sequence;
    }
    if (is_tagged_list(reg.exp, symbol.cond))
        goto ev_cond;
    if (is_tagged_list(reg.exp, symbol.let)) {
        reg.exp = let_to_combination(reg.exp);
        goto ev_application;
    }
    if (is_pair(reg.exp))
        goto ev_application;
    scheme_error("Unknown expression type", reg.exp);

ev_application:
    save(reg.cont);
    save(reg.env);
    reg.unev = cdr(reg.exp);
    save(reg.unev);
    reg.exp = car(reg.exp);
    reg.cont = make_label(EV_APPL_DID_OPERATOR);
    goto eval_dispatch;
ev_appl_did_operator:
    reg.unev = restore();
    reg.env = restore();
    reg.argl = EMPTY_LIST;
    reg.proc = reg.val;
    if (is_null(reg.unev))
        goto apply_dispatch;
    save(reg.proc);
ev_appl_operand_loop:
    save(reg.argl);
    reg.exp = car(reg.unev);
    if (is_null(cdr(reg.unev)))
        goto ev_appl_last_arg;
    save(reg.env);
    save(reg.unev);
    reg.cont = make_label(EV_APPL_ACCUMULATE_ARG);
    goto eval_dispatch;
ev_appl_accumulate_arg:
    reg.unev = restore();
    reg.env = restore();
    reg.argl = restore();
    /* The arguments are collected in reverse order. */
    reg.argl = cons(reg.val, reg.argl);
    reg.unev = cdr(reg.unev);
    goto ev_appl_operand_loop;
ev_appl_last_arg:
    reg.cont = make_label(EV_APPL_ACCUM_LAST_ARG);
    goto eval_dispatch;
ev_appl_accum_last_arg:
    reg.argl = restore();
    reg.argl = cons(reg.val, reg.argl);
    reg.argl = reverse_in_place(reg.argl);
    reg.proc = restore();

apply_dispatch:
    if (is_primitive(reg.proc)) {
        reg.val = apply_primitive_procedure(reg.proc, reg.argl);
        reg.cont = restore();
        goto go_to_continue;
    }
    if (is_procedure(reg.proc)) {
        reg.unev = procedure_parameters(reg.proc);
        reg.env = procedure_environment(reg.proc);
        reg.env = extend_environment(reg.unev, reg.argl, reg.env);
        reg.unev = procedure_body(reg.proc);
        goto ev_sequence;
    }
    scheme_error("The object is not applicable:", reg.proc);

    /* The caller has saved continue. */
ev_sequence:
    reg.exp = car(reg.unev);
    if (is_null(cdr(reg.unev))) {
        reg.cont = restore();
        goto eval_dispatch;
    }
    save(reg.unev);
    save(reg.env);
    reg.cont = make_label(EV_SEQUENCE_CONTINUE);
    goto eval_dispatch;
ev_sequence_continue:
    reg.env = restore();
    reg.unev = restore();
    reg.unev = cdr(reg.unev);
    goto ev_sequence;

ev_if:
    save(reg.exp);
    save(reg.env);
    save(reg.cont);
    reg.cont = make_label(EV_IF_DECIDE);
    reg.exp = cadr(reg.exp);
    goto eval_dispatch;
ev_if_decide:
    reg.cont = restore();
    reg.env = restore();
    reg.exp = restore();
    if (is_true(reg.val)) {
        reg.exp = caddr(reg.exp);
        goto eval_dispatch;
    }
    if (has_alternative(reg.exp)) {
        reg.exp = car(cdddr(reg.exp));
        goto eval_dispatch;
    }
    reg.val = UNSPECIFIED;
    goto go_to_continue;

    /* Clauses are tested one by one as in exercise 5.24. */
ev_cond:
    save(reg.cont);
    reg.unev = cdr(reg.exp);
ev_cond_loop:
    if (is_null(reg.unev)) {
        reg.val = UNSPECIFIED;
        reg.cont = restore();
        goto go_to_continue;
    }
    reg.exp = car(reg.unev);
    if (is_eq(car(reg.exp), symbol.else_)) {
        reg.unev = cdr(reg.exp);
        goto ev_sequence;
    }
    save(reg.unev);
    save(reg.env);
    reg.exp = car(reg.exp);
    reg.cont = make_label(EV_COND_DECIDE);
    goto eval_dispatch;
ev_cond_decide:
    reg.env = restore();
    reg.unev = restore();
    if (is_false(reg.val)) {
        reg.unev = cdr(reg.unev);
        goto ev_cond_loop;
    }
    reg.unev = cdar(reg.unev);
    if (is_pair(reg.unev))
        goto ev_sequence;
    reg.cont = restore();
    goto go_to_continue;

ev_assignment:
    reg.unev = cadr(reg.exp);
    save(reg.unev);
    reg.exp = caddr(reg.exp);
    save(reg.env);
    save(reg.cont);
    reg.cont = make_label(EV_ASSIGNMENT_1);
    goto eval_dispatch;
ev_assignment_1:
    reg.cont = restore();
    reg.env = restore();
    reg.unev = restore();
    set_variable_value(reg.unev, reg.val, reg.env);
    reg.val = symbol.ok;
    goto go_to_continue;

ev_definition:
    reg.unev = definition_variable(reg.exp);
    save(reg.unev);
    reg.exp = definition_value(reg.exp);
    save(reg.env);
    save(reg.cont);
    reg.cont = make_label(EV_DEFINITION_1);
    goto eval_dispatch;
ev_definition_1:
    reg.cont = restore();
    reg.env = restore();
    reg.unev = restore();
    define_variable(reg.unev, reg.val, reg.env);
    reg.val = symbol.ok;
    goto go_to_continue;

go_to_continue:
    switch ((Label) reg.cont.as.label) {
    case DONE:                   return;
    case EV_APPL_DID_OPERATOR:   goto ev_appl_did_operator;
    case EV_APPL_ACCUMULATE_ARG: goto ev_appl_accumulate_arg;
    case EV_APPL_ACCUM_LAST_ARG: goto ev_appl_accum_last_arg;
    case EV_SEQUENCE_CONTINUE:   goto ev_sequence_continue;
    case EV_IF_DECIDE:           goto ev_if_decide;
    case EV_COND_DECIDE:         goto ev_cond_decide;
    case EV_ASSIGNMENT_1:        goto ev_assignment_1;
    case EV_DEFINITION_1:        goto ev_definition_1;
    }
}

Value evaluate(Value exp, Value env)
{
    reg.exp = exp;
    reg.env = env;
    reg.cont = make_label(DONE);
    execute();
    return reg.val;
}
