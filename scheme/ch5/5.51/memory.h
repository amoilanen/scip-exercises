/* List-structured memory with a stop-and-copy garbage collector
 * (section 5.3), and the stack of the register machine.
 *
 * Any allocation may move every pair. A Value held in a C variable across
 * an allocation must therefore be reachable from a root: a register added
 * with add_root, the stack, or a variable registered with protect. */

#ifndef MEMORY_H
#define MEMORY_H

#include "object.h"

void init_memory(void);

Value cons(Value car, Value cdr);
Value make_cell(Type type, Value car, Value cdr);

Value car(Value pair);
Value cdr(Value pair);
void set_car(Value pair, Value value);
void set_cdr(Value pair, Value value);

/* Unchecked access to the two halves of a pair, procedure or other cell. */
Value cell_car(Value cell);
Value cell_cdr(Value cell);

static inline Value caar(Value v) { return car(car(v)); }
static inline Value cadr(Value v) { return car(cdr(v)); }
static inline Value cdar(Value v) { return cdr(car(v)); }
static inline Value cddr(Value v) { return cdr(cdr(v)); }
static inline Value caddr(Value v) { return car(cddr(v)); }
static inline Value cdddr(Value v) { return cdr(cddr(v)); }

void save(Value value);
Value restore(void);
void reset_stack(void);

void add_root(Value *location);
void protect(Value *location);
void unprotect(int count);
void reset_protection(void);

#endif
