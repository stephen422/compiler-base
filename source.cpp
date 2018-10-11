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

Source::Source(const Path &p) : path(p.path)
{
    std::ifstream in{path, std::ios::binary};
    if (!in) {
        std::cerr << path << ": " << strerror(errno) << std::endl;
        exit(EXIT_FAILURE);
    }

    // in.seekg(0, std::ios::end);
    // auto size = in.tellg();
    // in.seekg(0, std::ios::beg);

    // buf.resize(size);
    // in.read(buf.data(), size);

    line_off.push_back(0);

    // Read file line by line
    std::string line;
    while (std::getline(in, line)) {
        buf.insert(buf.cend(), line.cbegin(), line.cend());
        buf.push_back('\n');
        line_off.push_back(buf.cend() - buf.cbegin());
    }
}

Source::Source(const std::string &text)
    : buf(std::cbegin(text), std::cend(text)) {
    std::cout << "haha" << buf.size() << std::endl;
}

std::pair<int, int> Source::locate(size_t pos) const {
    // Search linearly for the current line.
    int line;
    for (line = 0; static_cast<size_t>(line) < line_off.size(); line++) {
        if (line_off[line] > pos)
            break;
    }
    int col = pos - line_off[line - 1] + 1;
    return {line, col};
}
