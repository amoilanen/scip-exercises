#include "printer.h"

#include <stdlib.h>
#include <string.h>

#include "memory.h"

static void print(FILE *out, Value v, bool write);

static void print_string(FILE *out, const char *text)
{
    fputc('"', out);
    for (; *text != '\0'; text++) {
        switch (*text) {
        case '"':  fputs("\\\"", out); break;
        case '\\': fputs("\\\\", out); break;
        case '\n': fputs("\\n", out); break;
        default:   fputc(*text, out);
        }
    }
    fputc('"', out);
}

/* Prints the shortest representation that reads back as the same number,
 * the way MIT Scheme does: 3. for 3.0 and .5 for 0.5. */
static void print_flonum(FILE *out, double x)
{
    char text[32];
    for (int precision = 1; precision <= 17; precision++) {
        snprintf(text, sizeof text, "%.*g", precision, x);
        if (strtod(text, NULL) == x)
            break;
    }
    const char *digits = text;
    if (text[0] == '-') {
        fputc('-', out);
        digits++;
    }
    if (digits[0] == '0' && digits[1] == '.')
        digits++;
    fputs(digits, out);
    if (strpbrk(digits, ".en") == NULL)
        fputc('.', out);
}

static void print_list(FILE *out, Value list, bool write)
{
    fputc('(', out);
    print(out, car(list), write);
    for (list = cdr(list); is_pair(list); list = cdr(list)) {
        fputc(' ', out);
        print(out, car(list), write);
    }
    if (!is_null(list)) {
        fputs(" . ", out);
        print(out, list, write);
    }
    fputc(')', out);
}

static void print(FILE *out, Value v, bool write)
{
    switch (v.type) {
    case TYPE_EMPTY_LIST:
        fputs("()", out);
        break;
    case TYPE_BOOLEAN:
        fputs(v.as.boolean ? "#t" : "#f", out);
        break;
    case TYPE_FIXNUM:
        fprintf(out, "%ld", v.as.fixnum);
        break;
    case TYPE_FLONUM:
        print_flonum(out, v.as.flonum);
        break;
    case TYPE_SYMBOL:
        fputs(symbol_name(v), out);
        break;
    case TYPE_STRING:
        if (write)
            print_string(out, string_text(v));
        else
            fputs(string_text(v), out);
        break;
    case TYPE_PAIR:
        print_list(out, v, write);
        break;
    case TYPE_PRIMITIVE:
        fprintf(out, "#[primitive-procedure %s]", v.as.primitive->name);
        break;
    case TYPE_PROCEDURE:
        fputs("#[compound-procedure]", out);
        break;
    case TYPE_COMPILED_PROCEDURE:
        fputs("#[compiled-procedure]", out);
        break;
    case TYPE_LABEL:
        fprintf(out, "#[label %d]", v.as.label);
        break;
    case TYPE_UNSPECIFIED:
        fputs("#!unspecific", out);
        break;
    case TYPE_EOF:
        fputs("#[eof]", out);
        break;
    case TYPE_BROKEN_HEART:
        fputs("#[broken-heart]", out);
        break;
    }
}

void write_value(FILE *out, Value v)
{
    print(out, v, true);
}

void display_value(FILE *out, Value v)
{
    print(out, v, false);
}
