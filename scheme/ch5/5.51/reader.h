#ifndef READER_H
#define READER_H

#include <stdio.h>

#include "object.h"

/* Both return END_OF_FILE when the input has no more data. */
Value read_from_file(FILE *file);
Value read_from_string(const char *text);

#endif
