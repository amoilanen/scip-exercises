#include "printer.h"

#include <math.h>
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

/* Prints the shortest digits that read back as the same number, in the
 * style of MIT Scheme: 3. for 3.0, .5 for 0.5 and 1e21 for 1e+21. */
static void print_flonum(FILE *out, double x)
{
    if (isnan(x) || isinf(x)) {
        fputs(isnan(x) ? "+nan.0" : x > 0 ? "+inf.0" : "-inf.0", out);
        return;
    }
    char text[32];
    for (int precision = 0; precision < 17; precision++) {
        snprintf(text, sizeof text, "%.*e", precision, x);
        if (strtod(text, NULL) == x)
            break;
    }

    /* text is [-]d.ddde[+-]xx; collect the significant digits. */
    char digits[32];
    int count = 0;
    const char *p = text;
    if (*p == '-') {
        fputc('-', out);
        p++;
    }
    for (; *p != 'e'; p++) {
        if (*p != '.')
            digits[count++] = *p;
    }
    int exponent = atoi(p + 1);
    while (count > 1 && digits[count - 1] == '0')
        count--;
    digits[count] = '\0';

    if (exponent >= 21 || exponent < -6) {
        fprintf(out, "%c%s%se%d", digits[0], count > 1 ? "." : "",
                digits + 1, exponent);
    } else if (exponent < 0) {
        fputc('.', out);
        for (int i = -1; i > exponent; i--)
            fputc('0', out);
        fputs(digits, out);
    } else {
        for (int i = 0; i <= exponent; i++)
            fputc(i < count ? digits[i] : '0', out);
        fputc('.', out);
        if (exponent + 1 < count)
            fputs(digits + exponent + 1, out);
    }
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
