# Builds build/NAME from build/NAME.c, a program that compile-to-c.scm
# compiled to C, linked with the runtime and its data layer. make runtime
# builds only the runtime.

CC = gcc
CFLAGS = -std=c99 -Wall -Wextra -pedantic -MMD -MP -I.
BUILD = build

RUNTIME = $(addprefix $(BUILD)/, object.o memory.o error.o reader.o \
            printer.o environment.o primitives.o runtime.o)

runtime: $(RUNTIME)

# A compiled program is one large function, which gcc is slow to optimize,
# and it spends most of its time in the runtime anyway.
$(BUILD)/%: $(BUILD)/%.c $(RUNTIME)
	$(CC) $(CFLAGS) -O0 -o $@ $< $(RUNTIME)

$(BUILD)/%.o: %.c | $(BUILD)
	$(CC) $(CFLAGS) -O2 -c -o $@ $<

$(BUILD):
	mkdir -p $@

clean:
	rm -rf $(BUILD)

.PHONY: runtime clean

-include $(RUNTIME:.o=.d)
