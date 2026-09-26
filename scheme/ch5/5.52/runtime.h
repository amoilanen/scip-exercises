/* The runtime for programs compiled to C by compile-to-c.scm. It adds
 * the registers and the operations of compiled code (section 5.5) to the
 * data layer of exercise 5.51. */

#ifndef RUNTIME_H
#define RUNTIME_H

#include "environment.h"
#include "error.h"
#include "memory.h"
#include "object.h"
#include "primitives.h"

extern struct Registers {
    Value env, proc, val, argl, cont;
} reg;

/* The compiled program. Its code starts running with env holding the
 * global environment. */
void run_program(void);

/* Reads the constants of the program from their printed representations
 * and keeps them safe from the garbage collector. */
void load_constants(Value *constants, const char *const *texts, size_t count);

Value make_compiled_procedure(Value entry, Value env);
Value compiled_procedure_entry(Value procedure);
Value compiled_procedure_env(Value procedure);

Value list1(Value item);
int label_of(Value label);

#endif
