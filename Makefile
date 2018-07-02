CXX ?= clang++
CXXFLAGS += -g -std=c++17 -pedantic -Wall -Wextra
PROG := cmp

$(PROG): main.o source.o lexer.o
	$(CXX) -o $(PROG) $^ $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

main.o: parse.h lexer.h
lexer.o: lexer.h source.h
source.o: source.h

%.s: %.cpp
	$(CXX) $(CXXFLAGS) -S $<

.PHONY: clean
clean:
	rm -f *.o *.s $(PROG)
