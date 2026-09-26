#include "primitives.h"

#include <limits.h>
#include <stdio.h>
#include <string.h>

#include "environment.h"
#include "error.h"
#include "memory.h"
#include "printer.h"

/*** Numbers ***/

static double to_double(Value v)
{
    if (is_fixnum(v))
        return (double) v.as.fixnum;
    if (!is_flonum(v))
        scheme_error("The object is not a number:", v);
    return v.as.flonum;
}

static long to_integer(Value v)
{
    if (!is_fixnum(v))
        scheme_error("The object is not an integer:", v);
    return v.as.fixnum;
}

static void integer_overflow(void)
{
    scheme_abort("Integer overflow");
}

static Value add(Value a, Value b)
{
    if (is_fixnum(a) && is_fixnum(b)) {
        long x = a.as.fixnum, y = b.as.fixnum;
        if ((y > 0 && x > LONG_MAX - y) || (y < 0 && x < LONG_MIN - y))
            integer_overflow();
        return make_fixnum(x + y);
    }
    return make_flonum(to_double(a) + to_double(b));
}

static Value subtract(Value a, Value b)
{
    if (is_fixnum(a) && is_fixnum(b)) {
        long x = a.as.fixnum, y = b.as.fixnum;
        if ((y < 0 && x > LONG_MAX + y) || (y > 0 && x < LONG_MIN + y))
            integer_overflow();
        return make_fixnum(x - y);
    }
    return make_flonum(to_double(a) - to_double(b));
}

static bool multiplication_overflows(long x, long y)
{
    if (x == 0 || y == 0)
        return false;
    if (x > 0)
        return y > 0 ? x > LONG_MAX / y : y < LONG_MIN / x;
    return y > 0 ? x < LONG_MIN / y : y < LONG_MAX / x;
}

static Value multiply(Value a, Value b)
{
    if (is_fixnum(a) && is_fixnum(b)) {
        long x = a.as.fixnum, y = b.as.fixnum;
        if (multiplication_overflows(x, y))
            integer_overflow();
        return make_fixnum(x * y);
    }
    return make_flonum(to_double(a) * to_double(b));
}

/* Integer division gives an integer only when it is exact. */
static Value divide(Value a, Value b)
{
    double divisor = to_double(b);
    if (divisor == 0)
        scheme_abort("Division by zero signalled by /.");
    if (is_fixnum(a) && is_fixnum(b)
        && !(a.as.fixnum == LONG_MIN && b.as.fixnum == -1)
        && a.as.fixnum % b.as.fixnum == 0)
        return make_fixnum(a.as.fixnum / b.as.fixnum);
    return make_flonum(to_double(a) / divisor);
}

static Value fold_numbers(Value (*operation)(Value, Value), Value initial,
                          Value arguments)
{
    for (; is_pair(arguments); arguments = cdr(arguments))
        initial = operation(initial, car(arguments));
    return initial;
}

static Value prim_add(Value args)
{
    return fold_numbers(add, make_fixnum(0), args);
}

static Value prim_multiply(Value args)
{
    return fold_numbers(multiply, make_fixnum(1), args);
}

static Value prim_subtract(Value args)
{
    if (is_null(cdr(args)))
        return subtract(make_fixnum(0), car(args));
    return fold_numbers(subtract, car(args), cdr(args));
}

static Value prim_divide(Value args)
{
    if (is_null(cdr(args)))
        return divide(make_fixnum(1), car(args));
    return fold_numbers(divide, car(args), cdr(args));
}

static Value integer_division(Value args, bool quotient)
{
    long x = to_integer(car(args)), y = to_integer(cadr(args));
    if (y == 0)
        scheme_abort("Division by zero signalled by integer division.");
    if (y == -1)
        return quotient ? subtract(make_fixnum(0), car(args)) : make_fixnum(0);
    return make_fixnum(quotient ? x / y : x % y);
}

static Value prim_quotient(Value args) { return integer_division(args, true); }
static Value prim_remainder(Value args)
{
    return integer_division(args, false);
}

static Value prim_abs(Value args)
{
    Value x = car(args);
    return to_double(x) < 0 ? subtract(make_fixnum(0), x) : x;
}

/* Returns a negative number, zero or a positive number when a is less
 * than, equal to or greater than b. */
static int compare(Value a, Value b)
{
    if (is_fixnum(a) && is_fixnum(b))
        return (a.as.fixnum > b.as.fixnum) - (a.as.fixnum < b.as.fixnum);
    double x = to_double(a), y = to_double(b);
    return (x > y) - (x < y);
}

static bool less(int order) { return order < 0; }
static bool greater(int order) { return order > 0; }
static bool equal(int order) { return order == 0; }
static bool less_or_equal(int order) { return order <= 0; }
static bool greater_or_equal(int order) { return order >= 0; }

static Value compare_all(Value args, bool (*relation)(int))
{
    if (!is_number(car(args)))
        scheme_error("The object is not a number:", car(args));
    bool holds = true;
    for (; is_pair(cdr(args)); args = cdr(args)) {
        if (!relation(compare(car(args), cadr(args))))
            holds = false;
    }
    return make_boolean(holds);
}

static Value prim_less(Value args) { return compare_all(args, less); }
static Value prim_greater(Value args) { return compare_all(args, greater); }
static Value prim_equal(Value args) { return compare_all(args, equal); }
static Value prim_less_or_equal(Value args)
{
    return compare_all(args, less_or_equal);
}
static Value prim_greater_or_equal(Value args)
{
    return compare_all(args, greater_or_equal);
}

/*** Pairs and lists ***/

static Value prim_car(Value args) { return car(car(args)); }
static Value prim_cdr(Value args) { return cdr(car(args)); }
static Value prim_caar(Value args) { return caar(car(args)); }
static Value prim_cadr(Value args) { return cadr(car(args)); }
static Value prim_cdar(Value args) { return cdar(car(args)); }
static Value prim_cddr(Value args) { return cddr(car(args)); }
static Value prim_caadr(Value args) { return car(cadr(car(args))); }
static Value prim_cdadr(Value args) { return cdr(cadr(car(args))); }
static Value prim_caddr(Value args) { return caddr(car(args)); }
static Value prim_cdddr(Value args) { return cdddr(car(args)); }
static Value prim_cadddr(Value args) { return car(cdddr(car(args))); }

static Value prim_cons(Value args) { return cons(car(args), cadr(args)); }

static Value prim_set_car(Value args)
{
    set_car(car(args), cadr(args));
    return UNSPECIFIED;
}

static Value prim_set_cdr(Value args)
{
    set_cdr(car(args), cadr(args));
    return UNSPECIFIED;
}

/* The argument list is always freshly made, so it can be the result. */
static Value prim_list(Value args) { return args; }

static Value prim_length(Value args)
{
    long length = 0;
    Value list = car(args);
    for (; is_pair(list); list = cdr(list))
        length++;
    if (!is_null(list))
        scheme_error("The object is not a list:", car(args));
    return make_fixnum(length);
}

/*** Predicates ***/

static bool is_equal(Value a, Value b)
{
    for (; is_pair(a) && is_pair(b); a = cdr(a), b = cdr(b)) {
        if (!is_equal(car(a), car(b)))
            return false;
    }
    if (is_string(a) && is_string(b))
        return strcmp(string_text(a), string_text(b)) == 0;
    return is_eq(a, b);
}

static Value prim_is_null(Value args)
{
    return make_boolean(is_null(car(args)));
}

static Value prim_is_pair(Value args)
{
    return make_boolean(is_pair(car(args)));
}

static Value prim_is_number(Value args)
{
    return make_boolean(is_number(car(args)));
}

static Value prim_is_symbol(Value args)
{
    return make_boolean(is_symbol(car(args)));
}

static Value prim_is_string(Value args)
{
    return make_boolean(is_string(car(args)));
}

static Value prim_is_procedure(Value args)
{
    Type type = car(args).type;
    return make_boolean(type == TYPE_PRIMITIVE || type == TYPE_PROCEDURE
                        || type == TYPE_COMPILED_PROCEDURE);
}

static Value prim_is_eq(Value args)
{
    return make_boolean(is_eq(car(args), cadr(args)));
}

static Value prim_is_equal(Value args)
{
    return make_boolean(is_equal(car(args), cadr(args)));
}

static Value prim_not(Value args) { return make_boolean(is_false(car(args))); }

/*** Input and output ***/

static Value prim_display(Value args)
{
    display_value(stdout, car(args));
    return UNSPECIFIED;
}

static Value prim_newline(Value args)
{
    (void) args;
    putchar('\n');
    return UNSPECIFIED;
}

static Value prim_error(Value args)
{
    if (is_string(car(args)))
        scheme_error_list(string_text(car(args)), cdr(args));
    else
        scheme_error_list("Error:", args);
    return UNSPECIFIED;
}

static const Primitive primitives[] = {
    { "car", 1, false, prim_car },
    { "cdr", 1, false, prim_cdr },
    { "caar", 1, false, prim_caar },
    { "cadr", 1, false, prim_cadr },
    { "cdar", 1, false, prim_cdar },
    { "cddr", 1, false, prim_cddr },
    { "caadr", 1, false, prim_caadr },
    { "cdadr", 1, false, prim_cdadr },
    { "caddr", 1, false, prim_caddr },
    { "cdddr", 1, false, prim_cdddr },
    { "cadddr", 1, false, prim_cadddr },
    { "cons", 2, false, prim_cons },
    { "set-car!", 2, false, prim_set_car },
    { "set-cdr!", 2, false, prim_set_cdr },
    { "list", 0, true, prim_list },
    { "length", 1, false, prim_length },
    { "null?", 1, false, prim_is_null },
    { "pair?", 1, false, prim_is_pair },
    { "number?", 1, false, prim_is_number },
    { "symbol?", 1, false, prim_is_symbol },
    { "string?", 1, false, prim_is_string },
    { "procedure?", 1, false, prim_is_procedure },
    { "eq?", 2, false, prim_is_eq },
    { "equal?", 2, false, prim_is_equal },
    { "not", 1, false, prim_not },
    { "+", 0, true, prim_add },
    { "-", 1, true, prim_subtract },
    { "*", 0, true, prim_multiply },
    { "/", 1, true, prim_divide },
    { "=", 1, true, prim_equal },
    { "<", 1, true, prim_less },
    { ">", 1, true, prim_greater },
    { "<=", 1, true, prim_less_or_equal },
    { ">=", 1, true, prim_greater_or_equal },
    { "quotient", 2, false, prim_quotient },
    { "remainder", 2, false, prim_remainder },
    { "abs", 1, false, prim_abs },
    { "display", 1, false, prim_display },
    { "newline", 0, false, prim_newline },
    { "error", 1, true, prim_error },
};

void define_primitives(Value env, const Primitive *table, size_t count)
{
    protect(&env);
    for (size_t i = 0; i < count; i++)
        define_variable(intern(table[i].name), make_primitive(&table[i]), env);
    unprotect(1);
}

Value setup_environment(void)
{
    Value env = extend_environment(EMPTY_LIST, EMPTY_LIST, EMPTY_LIST);
    protect(&env);
    define_primitives(env, primitives, sizeof primitives / sizeof *primitives);
    define_variable(intern("true"), TRUE_VALUE, env);
    define_variable(intern("false"), FALSE_VALUE, env);
    unprotect(1);
    return env;
}

Value apply_primitive_procedure(Value procedure, Value arguments)
{
    const Primitive *primitive = procedure.as.primitive;
    int count = 0;
    for (Value list = arguments; is_pair(list); list = cdr(list))
        count++;
    if (count < primitive->arity || (count > primitive->arity
                                     && !primitive->variadic))
        scheme_error("Wrong number of arguments passed to", procedure);
    return primitive->function(arguments);
}
