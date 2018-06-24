CXX ?= clang++
CXXFLAGS += -g -std=c++17 -pedantic -Wall -Wextra
PROG := cmp

$(PROG): main.o source.o lexer.o
	$(CXX) -o $(PROG) $^

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

main.o: parse.hpp lexer.hpp
lexer.o: lexer.hpp source.hpp
source.o: source.hpp

.PHONY: clean
clean:
	rm -f *.o $(PROG)
