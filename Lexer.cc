#include "Lexer.h"
#include <cctype>

void Lexer::Lex() {
  std::cout << "File size: " << src.buffer().size() << std::endl;
  std::cout << "Current pos: " << look - std::cbegin(sv) << std::endl;

	std::cout << "[" << *look << "]: ";
	// Identifier starts with an alnum or an underscore.
	if (std::isalnum(*look) || *look == '_') {
		std::cout << "ident\n";
	} else if (look == end()) {
		std::cout << "End of file\n";
	} else {
		std::cout << "not ident\n";
	}

	look++;
}
