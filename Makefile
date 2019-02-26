CXX ?= clang++
CXXFLAGS += -g -fsanitize=address -std=c++14 -Wall -Wextra
LDFLAGS := -fsanitize=address -fuse-ld=lld
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

main.o: sema.h parser.h ast.h lexer.h source.h
test.o: sema.h parser.h catch.hpp
sema.o: sema.h ast.h
parser.o: parser.h ast.h lexer.h source.h
lexer.o: lexer.h source.h string_view.h
ast.o: sema.h ast.h
source.o: source.h

%.s: %.cpp
	$(CXX) $(CXXFLAGS) -S $<

.PHONY: clean
clean:
	rm -f *.o *.s $(PROG)
