#ifndef EVAL_H
#define EVAL_H

#include "object.h"

void init_evaluator(void);

/* Runs the explicit-control evaluator on exp in env and returns the value. */
Value evaluate(Value exp, Value env);

#endif
