CXX ?= clang++
CXXFLAGS += -g -std=c++14 -Wall -Wextra -fsanitize=address
LDFLAGS := -fuse-ld=lld -fsanitize=address
PROG := cmp
OBJ := sema.o parser.o lexer.o ast.o source.o

$(PROG): main.o $(OBJ)
	$(CXX) -o $(PROG) $^ $(LDFLAGS)

debug: main.o $(OBJ)
	$(CXX) -o $(PROG) $^ $(LDFLAGS)

test: test.o $(OBJ)
	$(CXX) -o cmp-test $^ $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

main.o: sema.h symbol_table.cpp parser.h ast.h lexer.h source.h
test.o: sema.h symbol_table.cpp parser.h catch.hpp
sema.o: sema.h symbol_table.cpp ast.h
parser.o: parser.h ast.h lexer.h source.h
lexer.o: lexer.h source.h string_view.h
ast.o: sema.h symbol_table.cpp ast.h
source.o: source.h

%.s: %.cpp
	$(CXX) $(CXXFLAGS) -S $<

.PHONY: clean
clean:
	rm -f *.o *.s $(PROG)
