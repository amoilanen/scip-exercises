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

/* The halves of the cells, indexed by the pointers. They are defined
 * here so that the accessors below can be inline. */
extern Value *the_cars, *the_cdrs;

void wrong_type_pair(const char *operation, Value object);

/* Unchecked access to the two halves of a pair, procedure or other cell. */
static inline Value cell_car(Value cell) { return the_cars[cell.as.index]; }
static inline Value cell_cdr(Value cell) { return the_cdrs[cell.as.index]; }

static inline Value car(Value pair)
{
    if (!is_pair(pair))
        wrong_type_pair("car", pair);
    return cell_car(pair);
}

static inline Value cdr(Value pair)
{
    if (!is_pair(pair))
        wrong_type_pair("cdr", pair);
    return cell_cdr(pair);
}

void set_car(Value pair, Value value);
void set_cdr(Value pair, Value value);

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
void add_roots(Value *values, size_t count);
void protect(Value *location);
void unprotect(int count);
void reset_protection(void);

#endif
