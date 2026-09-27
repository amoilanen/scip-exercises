#include "error.h"

#include <stdio.h>

#include "memory.h"
#include "printer.h"

jmp_buf error_recovery;

static void begin_report(const char *message)
{
    fflush(stdout);
    fprintf(stderr, ";%s", message);
}

static void recover(void)
{
    fputc('\n', stderr);
    longjmp(error_recovery, 1);
}

void scheme_error(const char *message, Value irritant)
{
    begin_report(message);
    fputc(' ', stderr);
    write_value(stderr, irritant);
    recover();
}

void scheme_error_list(const char *message, Value irritants)
{
    begin_report(message);
    for (; is_pair(irritants); irritants = cdr(irritants)) {
        fputc(' ', stderr);
        write_value(stderr, car(irritants));
    }
    recover();
}

void scheme_abort(const char *message)
{
    begin_report(message);
    recover();
}
