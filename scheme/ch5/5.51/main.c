/* The driver loop: reads the expressions of a program from a file or from
 * standard input, evaluates them and prints their values. After an error
 * it goes on with the next expression, and exits with status 1 at the end. */

#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "eval.h"
#include "memory.h"
#include "primitives.h"
#include "printer.h"
#include "reader.h"

static Value the_global_environment;

static bool driver_loop(FILE *input)
{
    static bool failed = false;
    if (setjmp(error_recovery)) {
        failed = true;
        reset_evaluator();
        reset_protection();
    }
    for (;;) {
        Value exp = read_from_file(input);
        if (is_eof(exp))
            return !failed;
        Value val = evaluate(exp, the_global_environment);
        if (!is_unspecified(val)) {
            write_value(stdout, val);
            putchar('\n');
        }
    }
}

int main(int argc, char *argv[])
{
    if (argc > 2) {
        fprintf(stderr, "usage: %s [file]\n", argv[0]);
        return EXIT_FAILURE;
    }
    FILE *input = stdin;
    if (argc == 2 && (input = fopen(argv[1], "r")) == NULL) {
        perror(argv[1]);
        return EXIT_FAILURE;
    }

    init_memory();
    init_evaluator();
    add_root(&the_global_environment);
    the_global_environment = setup_environment();

    bool succeeded = driver_loop(input);
    fclose(input);
    return succeeded ? EXIT_SUCCESS : EXIT_FAILURE;
}
