#include "environment.h"

#include "error.h"
#include "memory.h"

static Value make_frame(Value variables, Value values)
{
    return cons(variables, values);
}

static Value frame_variables(Value frame) { return car(frame); }
static Value frame_values(Value frame) { return cdr(frame); }

static void add_binding_to_frame(Value variable, Value value, Value frame)
{
    protect(&frame);
    Value values = cons(value, frame_values(frame));
    set_cdr(frame, values);
    Value variables = cons(variable, frame_variables(frame));
    set_car(frame, variables);
    unprotect(1);
}

/* For (a b . rest) and (1 2 3 4) makes a frame binding a to 1, b to 2 and
 * rest to (3 4). */
static Value make_frame_with_rest(Value parameters, Value arguments)
{
    Value frame = make_frame(EMPTY_LIST, EMPTY_LIST);
    protect(&frame);
    protect(&parameters);
    protect(&arguments);
    for (; is_pair(parameters); parameters = cdr(parameters)) {
        add_binding_to_frame(car(parameters), car(arguments), frame);
        arguments = cdr(arguments);
    }
    add_binding_to_frame(parameters, arguments, frame);
    unprotect(3);
    return frame;
}

Value extend_environment(Value parameters, Value arguments, Value base)
{
    Value p = parameters, a = arguments;
    while (is_pair(p) && is_pair(a)) {
        p = cdr(p);
        a = cdr(a);
    }
    if (is_pair(p))
        scheme_error("Too few arguments supplied for", parameters);
    if (is_null(p) && !is_null(a))
        scheme_error("Too many arguments supplied for", parameters);
    if (!is_null(p) && !is_symbol(p))
        scheme_error("Bad parameter list", parameters);

    protect(&base);
    Value frame = is_null(p) ? make_frame(parameters, arguments)
                             : make_frame_with_rest(parameters, arguments);
    unprotect(1);
    return cons(frame, base);
}

/* Returns the pair of the frame's value list that holds the variable's
 * value, or the empty list if the frame has no binding for it. */
static Value find_in_frame(Value variable, Value frame)
{
    Value variables = frame_variables(frame);
    Value values = frame_values(frame);
    for (; is_pair(variables); variables = cdr(variables)) {
        if (is_eq(car(variables), variable))
            return values;
        values = cdr(values);
    }
    return EMPTY_LIST;
}

static Value find_binding(Value variable, Value env, const char *message)
{
    for (; is_pair(env); env = cdr(env)) {
        Value cell = find_in_frame(variable, car(env));
        if (is_pair(cell))
            return cell;
    }
    scheme_error(message, variable);
    return EMPTY_LIST;
}

Value lookup_variable_value(Value variable, Value env)
{
    return car(find_binding(variable, env, "Unbound variable"));
}

void set_variable_value(Value variable, Value value, Value env)
{
    set_car(find_binding(variable, env, "Unbound variable -- SET!"), value);
}

void define_variable(Value variable, Value value, Value env)
{
    Value frame = car(env);
    Value cell = find_in_frame(variable, frame);
    if (is_pair(cell))
        set_car(cell, value);
    else
        add_binding_to_frame(variable, value, frame);
}
