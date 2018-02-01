CFLAGS += -g -Wall -Wextra
PROG := cmp

$(PROG): main.o lexer.o
	$(CC) -o $(PROG) $^

.c.o:
	$(CC) $(CFLAGS) -c -o $@ $<

main.o: main.c lexer.h
lexer.o: lexer.c lexer.h

.PHONY: clean
clean:
	rm -f *.o $(PROG)
