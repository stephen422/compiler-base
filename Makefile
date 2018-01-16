CFLAGS ?= -g -Wall
PROG := cmp

all: main.o lexer.o
	$(CC) -o $(PROG) $^

.c.o:
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: clean
clean:
	rm -f *.o $(PROG)
