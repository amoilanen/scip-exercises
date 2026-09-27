#ifndef PRINTER_H
#define PRINTER_H

#include <stdio.h>

#include "object.h"

/* write prints strings in double quotes, so that the reader can read them
 * back; display prints their characters only. */
void write_value(FILE *out, Value v);
void display_value(FILE *out, Value v);

#endif
