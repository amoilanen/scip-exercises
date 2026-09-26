/* Scheme objects as typed pointers (section 5.3.1).
 *
 * A Value is a type tag plus a datum. Numbers, booleans, symbols and the
 * other atoms are held in the Value itself. Pairs and procedures point into
 * the list-structured memory of memory.c by index. */

#ifndef OBJECT_H
#define OBJECT_H

#include <stdbool.h>
#include <stddef.h>

typedef enum {
    TYPE_EMPTY_LIST,
    TYPE_BOOLEAN,
    TYPE_FIXNUM,
    TYPE_FLONUM,
    TYPE_SYMBOL,
    TYPE_STRING,
    TYPE_PRIMITIVE,
    TYPE_LABEL,
    TYPE_UNSPECIFIED,
    TYPE_EOF,
    /* The types below point into memory; the garbage collector moves them. */
    TYPE_PAIR,
    TYPE_PROCEDURE,
    TYPE_COMPILED_PROCEDURE,
    TYPE_BROKEN_HEART
} Type;

struct Primitive;

typedef struct {
    Type type;
    union {
        bool boolean;
        long fixnum;
        double flonum;
        size_t index;
        const struct Primitive *primitive;
        int label;
    } as;
} Value;

typedef struct Primitive {
    const char *name;
    int arity;       /* the number of arguments, or the minimum if variadic */
    bool variadic;
    Value (*function)(Value arguments);
} Primitive;

extern const Value EMPTY_LIST;
extern const Value TRUE_VALUE;
extern const Value FALSE_VALUE;
extern const Value UNSPECIFIED;
extern const Value END_OF_FILE;

Value make_boolean(bool b);
Value make_fixnum(long n);
Value make_flonum(double x);
Value make_primitive(const Primitive *primitive);
Value make_label(int label);
Value make_string(const char *text);
Value intern(const char *name);

const char *symbol_name(Value symbol);
const char *string_text(Value string);

static inline bool is_null(Value v) { return v.type == TYPE_EMPTY_LIST; }
static inline bool is_boolean(Value v) { return v.type == TYPE_BOOLEAN; }
static inline bool is_fixnum(Value v) { return v.type == TYPE_FIXNUM; }
static inline bool is_flonum(Value v) { return v.type == TYPE_FLONUM; }
static inline bool is_number(Value v) { return is_fixnum(v) || is_flonum(v); }
static inline bool is_symbol(Value v) { return v.type == TYPE_SYMBOL; }
static inline bool is_string(Value v) { return v.type == TYPE_STRING; }
static inline bool is_pair(Value v) { return v.type == TYPE_PAIR; }
static inline bool is_primitive(Value v) { return v.type == TYPE_PRIMITIVE; }
static inline bool is_procedure(Value v) { return v.type == TYPE_PROCEDURE; }
static inline bool is_unspecified(Value v)
{
    return v.type == TYPE_UNSPECIFIED;
}
static inline bool is_eof(Value v) { return v.type == TYPE_EOF; }
static inline bool is_false(Value v) { return is_boolean(v) && !v.as.boolean; }
static inline bool is_true(Value v) { return !is_false(v); }

static inline bool is_pointer(Value v)
{
    return v.type == TYPE_PAIR || v.type == TYPE_PROCEDURE
        || v.type == TYPE_COMPILED_PROCEDURE;
}

bool is_eq(Value a, Value b);

#endif
