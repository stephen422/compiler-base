CFLAGS += -g -std=c99 -pedantic -Wall -Wextra
PROG := cmp

$(PROG): main.o lexer.o
	$(CC) -o $(PROG) $^

.SUFFIXES: .c .o
.c.o:
	$(CC) $(CFLAGS) -c -o $@ $<

main.o: lexer.h
lexer.o: lexer.h

.PHONY: clean
clean:
	rm -f *.o $(PROG)
