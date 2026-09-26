#include "object.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const Value EMPTY_LIST = { TYPE_EMPTY_LIST, { 0 } };
const Value TRUE_VALUE = { TYPE_BOOLEAN, { true } };
const Value FALSE_VALUE = { TYPE_BOOLEAN, { false } };
const Value UNSPECIFIED = { TYPE_UNSPECIFIED, { 0 } };
const Value END_OF_FILE = { TYPE_EOF, { 0 } };

/* Symbol names and string contents live outside the garbage-collected
 * memory. Strings come only from the reader, so they are never freed. */
typedef struct {
    char **items;
    size_t count;
    size_t capacity;
} TextTable;

static TextTable symbols;
static TextTable strings;

static void *allocate(size_t size)
{
    void *block = malloc(size);
    if (block == NULL) {
        fputs("Out of memory\n", stderr);
        exit(EXIT_FAILURE);
    }
    return block;
}

static size_t add_text(TextTable *table, const char *text)
{
    if (table->count == table->capacity) {
        size_t capacity = table->capacity ? 2 * table->capacity : 64;
        char **items = allocate(capacity * sizeof *items);
        if (table->count > 0)
            memcpy(items, table->items, table->count * sizeof *items);
        free(table->items);
        table->items = items;
        table->capacity = capacity;
    }
    table->items[table->count] = allocate(strlen(text) + 1);
    strcpy(table->items[table->count], text);
    return table->count++;
}

Value make_boolean(bool b)
{
    return b ? TRUE_VALUE : FALSE_VALUE;
}

Value make_fixnum(long n)
{
    Value v = { TYPE_FIXNUM, { 0 } };
    v.as.fixnum = n;
    return v;
}

Value make_flonum(double x)
{
    Value v = { TYPE_FLONUM, { 0 } };
    v.as.flonum = x;
    return v;
}

Value make_primitive(const Primitive *primitive)
{
    Value v = { TYPE_PRIMITIVE, { 0 } };
    v.as.primitive = primitive;
    return v;
}

Value make_label(int label)
{
    Value v = { TYPE_LABEL, { 0 } };
    v.as.label = label;
    return v;
}

Value make_string(const char *text)
{
    Value v = { TYPE_STRING, { 0 } };
    v.as.index = add_text(&strings, text);
    return v;
}

Value intern(const char *name)
{
    Value v = { TYPE_SYMBOL, { 0 } };
    for (size_t i = 0; i < symbols.count; i++) {
        if (strcmp(symbols.items[i], name) == 0) {
            v.as.index = i;
            return v;
        }
    }
    v.as.index = add_text(&symbols, name);
    return v;
}

const char *symbol_name(Value symbol)
{
    return symbols.items[symbol.as.index];
}

const char *string_text(Value string)
{
    return strings.items[string.as.index];
}

bool is_eq(Value a, Value b)
{
    if (a.type != b.type)
        return false;
    switch (a.type) {
    case TYPE_EMPTY_LIST:
    case TYPE_UNSPECIFIED:
    case TYPE_EOF:
        return true;
    case TYPE_BOOLEAN:
        return a.as.boolean == b.as.boolean;
    case TYPE_FIXNUM:
        return a.as.fixnum == b.as.fixnum;
    case TYPE_FLONUM:
        return a.as.flonum == b.as.flonum;
    case TYPE_PRIMITIVE:
        return a.as.primitive == b.as.primitive;
    case TYPE_LABEL:
        return a.as.label == b.as.label;
    default:
        return a.as.index == b.as.index;
    }
}
