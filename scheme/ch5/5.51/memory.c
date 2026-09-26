#include "memory.h"

#include <stdio.h>
#include <stdlib.h>

#include "error.h"

#ifndef MEMORY_SIZE
#define MEMORY_SIZE (1 << 18)
#endif

#ifndef STACK_SIZE
#define STACK_SIZE 100000
#endif

#define MAX_ROOTS 32
#define MAX_PROTECTED 1024

static Value *the_cars, *the_cdrs;
static Value *new_cars, *new_cdrs;
static size_t free_cell;

static Value stack[STACK_SIZE];
static size_t stack_top;

static Value *roots[MAX_ROOTS];
static int root_count;
static Value *protected[MAX_PROTECTED];
static int protected_count;

static Value *allocate_cells(void)
{
    Value *cells = malloc(MEMORY_SIZE * sizeof *cells);
    if (cells == NULL) {
        fputs("Cannot allocate the Scheme memory\n", stderr);
        exit(EXIT_FAILURE);
    }
    return cells;
}

void init_memory(void)
{
    the_cars = allocate_cells();
    the_cdrs = allocate_cells();
    new_cars = allocate_cells();
    new_cdrs = allocate_cells();
}

/* Moves the object v points to into new space, unless it has been moved
 * already, in which case a broken heart in its car and the forwarding
 * address in its cdr tell where it went. */
static Value relocate(Value v)
{
    if (!is_pointer(v))
        return v;
    size_t old = v.as.index;
    if (the_cars[old].type != TYPE_BROKEN_HEART) {
        new_cars[free_cell] = the_cars[old];
        new_cdrs[free_cell] = the_cdrs[old];
        the_cars[old].type = TYPE_BROKEN_HEART;
        the_cdrs[old].as.index = free_cell++;
    }
    v.as.index = the_cdrs[old].as.index;
    return v;
}

static void collect_garbage(Value *car, Value *cdr)
{
    free_cell = 0;
    *car = relocate(*car);
    *cdr = relocate(*cdr);
    for (int i = 0; i < root_count; i++)
        *roots[i] = relocate(*roots[i]);
    for (int i = 0; i < protected_count; i++)
        *protected[i] = relocate(*protected[i]);
    for (size_t i = 0; i < stack_top; i++)
        stack[i] = relocate(stack[i]);

    for (size_t scan = 0; scan < free_cell; scan++) {
        new_cars[scan] = relocate(new_cars[scan]);
        new_cdrs[scan] = relocate(new_cdrs[scan]);
    }

    Value *swap = the_cars;
    the_cars = new_cars;
    new_cars = swap;
    swap = the_cdrs;
    the_cdrs = new_cdrs;
    new_cdrs = swap;
}

Value make_cell(Type type, Value car, Value cdr)
{
    if (free_cell == MEMORY_SIZE) {
        collect_garbage(&car, &cdr);
        if (free_cell == MEMORY_SIZE)
            scheme_abort("Aborting!: out of memory");
    }
    the_cars[free_cell] = car;
    the_cdrs[free_cell] = cdr;
    Value cell = { type, { 0 } };
    cell.as.index = free_cell++;
    return cell;
}

Value cons(Value car, Value cdr)
{
    return make_cell(TYPE_PAIR, car, cdr);
}

Value cell_car(Value cell)
{
    return the_cars[cell.as.index];
}

Value cell_cdr(Value cell)
{
    return the_cdrs[cell.as.index];
}

static void check_pair(const char *operation, Value v)
{
    if (!is_pair(v))
        scheme_error(operation, v);
}

Value car(Value pair)
{
    check_pair("The object passed to car is not a pair:", pair);
    return the_cars[pair.as.index];
}

Value cdr(Value pair)
{
    check_pair("The object passed to cdr is not a pair:", pair);
    return the_cdrs[pair.as.index];
}

void set_car(Value pair, Value value)
{
    check_pair("The object passed to set-car! is not a pair:", pair);
    the_cars[pair.as.index] = value;
}

void set_cdr(Value pair, Value value)
{
    check_pair("The object passed to set-cdr! is not a pair:", pair);
    the_cdrs[pair.as.index] = value;
}

void save(Value value)
{
    if (stack_top == STACK_SIZE)
        scheme_abort("Aborting!: maximum recursion depth exceeded");
    stack[stack_top++] = value;
}

Value restore(void)
{
    return stack[--stack_top];
}

void reset_stack(void)
{
    stack_top = 0;
}

void add_root(Value *location)
{
    if (root_count == MAX_ROOTS) {
        fputs("Too many roots\n", stderr);
        exit(EXIT_FAILURE);
    }
    roots[root_count++] = location;
}

void protect(Value *location)
{
    if (protected_count == MAX_PROTECTED)
        scheme_abort("Aborting!: nesting too deep");
    protected[protected_count++] = location;
}

void unprotect(int count)
{
    protected_count -= count;
}

void reset_protection(void)
{
    protected_count = 0;
}
