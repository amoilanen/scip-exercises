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
    return is_symbol(cadr(exp)) ? cadr(exp) : caadr(exp);
}

static Value definition_value(Value exp)
{
    if (is_symbol(cadr(exp)))
        return caddr(exp);
    return make_lambda(cdar(cdr(exp)), cddr(exp));
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
