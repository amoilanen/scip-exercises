#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include <stddef.h>

#include "object.h"

/* Makes a global environment with the primitive procedures and the
 * variables true and false. */
Value setup_environment(void);

void define_primitives(Value env, const Primitive *table, size_t count);
Value apply_primitive_procedure(Value procedure, Value arguments);

#endif
