#include "runtime.h"

#include <stdio.h>
#include <stdlib.h>

#include "reader.h"

struct Registers reg;

void load_constants(Value *constants, const char *const *texts, size_t count)
{
    for (size_t i = 0; i < count; i++)
        constants[i] = EMPTY_LIST;
    add_roots(constants, count);
    for (size_t i = 0; i < count; i++)
        constants[i] = read_from_string(texts[i]);
}

Value make_compiled_procedure(Value entry, Value env)
{
    return make_cell(TYPE_COMPILED_PROCEDURE, entry, env);
}

static void check_compiled_procedure(Value procedure)
{
    if (procedure.type != TYPE_COMPILED_PROCEDURE)
        scheme_error("The object is not applicable:", procedure);
}

Value compiled_procedure_entry(Value procedure)
{
    check_compiled_procedure(procedure);
    return cell_car(procedure);
}

Value compiled_procedure_env(Value procedure)
{
    check_compiled_procedure(procedure);
    return cell_cdr(procedure);
}

Value list1(Value item)
{
    return cons(item, EMPTY_LIST);
}

int label_of(Value label)
{
    if (label.type != TYPE_LABEL)
        scheme_error("Not a label:", label);
    return label.as.label;
}

/*** Primitives needed by the metacircular evaluator ***/

static Value prim_read(Value args)
{
    (void) args;
    return read_from_file(stdin);
}

static Value prim_is_eof_object(Value args)
{
    return make_boolean(is_eof(car(args)));
}

/* A primitive cannot run compiled code, so apply takes only primitive
 * procedures. That is all the metacircular evaluator needs. */
static Value prim_apply(Value args)
{
    Value procedure = car(args);
    if (!is_primitive(procedure))
        scheme_error("apply takes only primitive procedures:", procedure);
    return apply_primitive_procedure(procedure, cadr(args));
}

static const Primitive runtime_primitives[] = {
    { "read", 0, false, prim_read },
    { "eof-object?", 1, false, prim_is_eof_object },
    { "apply", 2, false, prim_apply },
};

/* An error ends the program with status 1. */
int main(void)
{
    init_memory();
    add_root(&reg.env);
    add_root(&reg.proc);
    add_root(&reg.val);
    add_root(&reg.argl);
    add_root(&reg.cont);
    if (setjmp(error_recovery))
        return EXIT_FAILURE;

    reg.env = setup_environment();
    define_primitives(reg.env, runtime_primitives,
                      sizeof runtime_primitives / sizeof *runtime_primitives);
    run_program();
    return EXIT_SUCCESS;
}
