# Builds build/scheme, the Scheme interpreter of exercise 5.51.

CC = gcc
CFLAGS = -std=c99 -Wall -Wextra -pedantic -O2 -MMD -MP
BUILD = build

OBJECTS = $(addprefix $(BUILD)/, object.o memory.o error.o reader.o \
            printer.o environment.o primitives.o eval.o main.o)

$(BUILD)/scheme: $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^

$(BUILD)/%.o: %.c | $(BUILD)
	$(CC) $(CFLAGS) -c -o $@ $<

$(BUILD):
	mkdir -p $@

clean:
	rm -rf $(BUILD)

.PHONY: clean

-include $(OBJECTS:.o=.d)
