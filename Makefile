CXXFLAGS += -g -std=c++17 -pedantic -Wall -Wextra
PROG := cmp

$(PROG): main.o source.o lexer.o
	$(CXX) -o $(PROG) $^

%.o: %.cc
	$(CXX) $(CXXFLAGS) -c -o $@ $<

main.o: parse.hh lexer.hh
lexer.o: lexer.hh source.hh
source.o: source.hh

.PHONY: clean
clean:
	rm -f *.o $(PROG)
