# Builds build/NAME from build/NAME.c, a program that compile-to-c.scm
# compiled to C, linked with the runtime and the data layer of exercise
# 5.51. make runtime builds only the runtime.

CC = gcc
CORE = ../5.51
CFLAGS = -std=c99 -Wall -Wextra -pedantic -O2 -MMD -MP -I. -I$(CORE)
BUILD = build

RUNTIME = $(addprefix $(BUILD)/, object.o memory.o error.o reader.o \
            printer.o environment.o primitives.o runtime.o)

vpath %.c $(CORE)

runtime: $(RUNTIME)

$(BUILD)/%: $(BUILD)/%.c $(RUNTIME)
	$(CC) $(CFLAGS) -o $@ $< $(RUNTIME)

$(BUILD)/%.o: %.c | $(BUILD)
	$(CC) $(CFLAGS) -c -o $@ $<

$(BUILD):
	mkdir -p $@

clean:
	rm -rf $(BUILD)

.PHONY: runtime clean

-include $(RUNTIME:.o=.d)
