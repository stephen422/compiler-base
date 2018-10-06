CXX ?= clang++
CXXFLAGS += -g -fsanitize=address -std=c++14 -pedantic -Wall -Wextra
PROG := cmp

$(PROG): main.o parser.o lexer.o source.o
	$(CXX) -fsanitize=address -o $(PROG) $^ $(LDFLAGS)

debug: main.o parser.o lexer.o source.o
	$(CXX) -fsanitize=address -o $(PROG) $^ $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

main.o: parser.h lexer.h
parser.o: parser.h lexer.h
lexer.o: lexer.h source.h string_view.h
source.o: source.h

%.s: %.cpp
	$(CXX) $(CXXFLAGS) -S $<

.PHONY: clean
clean:
	rm -f *.o *.s $(PROG)
