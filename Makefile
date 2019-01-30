CXX := clang++
CXXFLAGS += -g -fsanitize=address,leak -std=c++14 -Wall -Wextra
PROG := cmp
OBJ := parser.o lexer.o ast.o source.o

$(PROG): main.o $(OBJ)
	$(CXX) -fsanitize=address,leak -o $(PROG) $^ $(LDFLAGS)

debug: main.o $(OBJ)
	$(CXX) -fsanitize=address,leak -o $(PROG) $^ $(LDFLAGS)

test: test.o $(OBJ)
	$(CXX) -fsanitize=address,leak -o cmp-test $^ $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

main.o: parser.h lexer.h
test.o: parser.h catch.hpp
parser.o: parser.h lexer.h ast.h
lexer.o: lexer.h source.h string_view.h
ast.o: ast.h
source.o: source.h

%.s: %.cpp
	$(CXX) $(CXXFLAGS) -S $<

.PHONY: clean
clean:
	rm -f *.o *.s $(PROG)
