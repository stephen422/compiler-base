#include "source.h"
#include <cstring>
#include <fstream>
#include <sstream>
#include <vector>

void use_stringstream(std::ifstream& in)
{
    std::stringstream sstr;
    sstr << in.rdbuf();
}

Source::Source(const Path &p) : filename(p.path)
{
    std::ifstream in{filename, std::ios::binary};
    if (!in) {
        std::cerr << filename << ": " << strerror(errno) << std::endl;
        exit(EXIT_FAILURE);
    }
    init(in);
}

Source::Source(const std::string &text) : filename("(none)") {
    std::stringstream ss{text};
    init(ss);
}

void Source::init(std::istream &in) {
    line_off.push_back(0);

    // Read file line by line.
    std::string line;
    while (std::getline(in, line)) {
        buf.insert(buf.cend(), line.cbegin(), line.cend());
        // TODO: Always assumes that the source text ends with a newline, which
        // may not be true especially for short unit-test texts.
        buf.push_back('\n');
        line_off.push_back(buf.cend() - buf.cbegin());
    }
}

std::pair<int, int> Source::locate(size_t pos) const {
    // Search linearly for the current line.
    // TODO: flaky.
    int line;
    for (line = 0; static_cast<size_t>(line) < line_off.size(); line++) {
        if (line_off[line] > pos)
            break;
    }
    int col = pos - line_off[line - 1] + 1;
    return {line, col};
}
