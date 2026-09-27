/* Environments as lists of frames, each frame a pair of a list of
 * variables and a list of their values (section 4.1.3). */

#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "object.h"

/* The parameters may end in a dotted rest parameter, as in (a b . rest). */
Value extend_environment(Value parameters, Value arguments, Value base);
Value lookup_variable_value(Value variable, Value env);
void set_variable_value(Value variable, Value value, Value env);
void define_variable(Value variable, Value value, Value env);

#endif
