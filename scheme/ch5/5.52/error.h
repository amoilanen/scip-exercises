/* Errors print a message on stderr and jump back to error_recovery, which
 * main sets up with setjmp. */

#ifndef ERROR_H
#define ERROR_H

#include <setjmp.h>

#include "object.h"

extern jmp_buf error_recovery;

void scheme_error(const char *message, Value irritant);
void scheme_error_list(const char *message, Value irritants);
void scheme_abort(const char *message);

#endif
