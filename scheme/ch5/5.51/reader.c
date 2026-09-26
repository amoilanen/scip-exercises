#include "reader.h"

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "memory.h"

#define MAX_TOKEN 256

/* Characters come from a file or from a string. */
typedef struct {
    FILE *file;
    const char *text;
} Source;

static int next_char(Source *in)
{
    if (in->file != NULL)
        return getc(in->file);
    return *in->text == '\0' ? EOF : (unsigned char) *in->text++;
}

static void unread_char(Source *in, int c)
{
    if (c == EOF)
        return;
    if (in->file != NULL)
        ungetc(c, in->file);
    else
        in->text--;
}

static int peek_char(Source *in)
{
    int c = next_char(in);
    unread_char(in, c);
    return c;
}

static bool is_delimiter(int c)
{
    return c == EOF || isspace(c) || strchr("()\";'", c) != NULL;
}

/* Skips whitespace and comments and returns the first character after
 * them. */
static int skip_atmosphere(Source *in)
{
    for (;;) {
        int c = next_char(in);
        if (c == ';') {
            while (c != '\n' && c != EOF)
                c = next_char(in);
        }
        if (c == EOF || !isspace(c))
            return c;
    }
}

static Value read_datum(Source *in);
static Value read_datum_starting_with(Source *in, int c);

static Value read_list(Source *in)
{
    Value head = EMPTY_LIST, last = EMPTY_LIST;
    protect(&head);
    protect(&last);
    for (;;) {
        int c = skip_atmosphere(in);
        if (c == EOF)
            scheme_abort("Unexpected end of input in a list");
        if (c == ')')
            break;
        if (c == '.' && is_delimiter(peek_char(in))) {
            if (!is_pair(last))
                scheme_abort("Nothing before . in a dotted list");
            Value tail = read_datum(in);
            set_cdr(last, tail);
            if (skip_atmosphere(in) != ')')
                scheme_abort("Expected ) after the tail of a dotted list");
            break;
        }
        Value cell = cons(read_datum_starting_with(in, c), EMPTY_LIST);
        if (is_null(head))
            head = cell;
        else
            set_cdr(last, cell);
        last = cell;
    }
    unprotect(2);
    return head;
}

static Value read_quotation(Source *in)
{
    Value quoted = read_datum(in);
    if (is_eof(quoted))
        scheme_abort("Unexpected end of input after '");
    return cons(intern("quote"), cons(quoted, EMPTY_LIST));
}

static Value read_string(Source *in)
{
    static char *buffer;
    static size_t capacity;
    size_t length = 0;
    for (;;) {
        int c = next_char(in);
        if (c == EOF)
            scheme_abort("Unexpected end of input in a string");
        if (c == '"')
            break;
        if (c == '\\') {
            c = next_char(in);
            if (c == 'n')
                c = '\n';
            else if (c == 't')
                c = '\t';
            else if (c == EOF)
                scheme_abort("Unexpected end of input in a string");
        }
        if (length + 1 >= capacity) {
            capacity = capacity ? 2 * capacity : 256;
            char *grown = realloc(buffer, capacity);
            if (grown == NULL)
                scheme_abort("Aborting!: out of memory");
            buffer = grown;
        }
        buffer[length++] = (char) c;
    }
    buffer[length] = '\0';
    return make_string(length > 0 ? buffer : "");
}

/* Reads the rest of a token whose first character, c, was read already. */
static void read_token(Source *in, int c, char token[MAX_TOKEN])
{
    size_t length = 0;
    while (!is_delimiter(c)) {
        if (length == MAX_TOKEN - 1)
            scheme_abort("Token too long");
        token[length++] = (char) c;
        c = next_char(in);
    }
    unread_char(in, c);
    token[length] = '\0';
}

static bool parse_number(const char *token, Value *number)
{
    if (strspn(token, "0123456789+-.e") != strlen(token)
        || strpbrk(token, "0123456789") == NULL
        || strchr("0123456789+-.", token[0]) == NULL)
        return false;

    char *end;
    errno = 0;
    long n = strtol(token, &end, 10);
    if (*end == '\0' && errno == 0) {
        *number = make_fixnum(n);
        return true;
    }
    double x = strtod(token, &end);
    if (*end == '\0') {
        *number = make_flonum(x);
        return true;
    }
    return false;
}

static Value read_hash_syntax(Source *in)
{
    char token[MAX_TOKEN];
    read_token(in, '#', token);
    if (strcmp(token, "#t") == 0 || strcmp(token, "#true") == 0)
        return TRUE_VALUE;
    if (strcmp(token, "#f") == 0 || strcmp(token, "#false") == 0)
        return FALSE_VALUE;
    scheme_error("Unknown syntax", make_string(token));
    return UNSPECIFIED;
}

static Value read_datum_starting_with(Source *in, int c)
{
    switch (c) {
    case '(':
        return read_list(in);
    case ')':
        scheme_abort("Unexpected )");
        return UNSPECIFIED;
    case '\'':
        return read_quotation(in);
    case '"':
        return read_string(in);
    case '#':
        return read_hash_syntax(in);
    default: {
        char token[MAX_TOKEN];
        Value number;
        read_token(in, c, token);
        return parse_number(token, &number) ? number : intern(token);
    }
    }
}

static Value read_datum(Source *in)
{
    int c = skip_atmosphere(in);
    return c == EOF ? END_OF_FILE : read_datum_starting_with(in, c);
}

Value read_from_file(FILE *file)
{
    Source in = { file, NULL };
    return read_datum(&in);
}

Value read_from_string(const char *text)
{
    Source in = { NULL, text };
    return read_datum(&in);
}
