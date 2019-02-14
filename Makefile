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

main.o: parser.h lexer.h
test.o: parser.h catch.hpp
sema.o: sema.h
parser.o: parser.h lexer.h ast.h
lexer.o: lexer.h source.h string_view.h
ast.o: ast.h
source.o: source.h

%.s: %.cpp
	$(CXX) $(CXXFLAGS) -S $<

.PHONY: clean
clean:
	rm -f *.o *.s $(PROG)
