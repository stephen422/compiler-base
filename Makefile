CFLAGS += -g -std=c99 -fsanitize=address,leak,undefined -pedantic -Wall -Wextra
PROG := cmp

$(PROG): main.o parser.o lexer.o
	$(CC) $(CFLAGS) -o $(PROG) $^

.SUFFIXES: .c .o
.c.o:
	$(CC) $(CFLAGS) -c -o $@ $<

main.o: parser.h lexer.h
parser.o: parser.h lexer.h
lexer.o: lexer.h

.PHONY: clean
clean:
	rm -f *.o $(PROG)
