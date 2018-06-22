CXXFLAGS += -g -std=c++17 -pedantic -Wall -Wextra
PROG := cmp

$(PROG): main.o source.o lexer.o
	$(CXX) -o $(PROG) $^

.cc.o:
	$(CXX) $(CXXFLAGS) -c -o $@ $<

main.cc: parse.hh lexer.hh
lexer.o: lexer.cc lexer.hh

.PHONY: clean
clean:
	rm -f *.o $(PROG)
