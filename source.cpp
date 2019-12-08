#include "source.h"
#include <cstring>
#include <fstream>
#include <sstream>
#include <vector>
#include "fmt/core.h"

namespace cmp {

Source::Source(const Path &p) : filename(p.path)
{
    std::ifstream in{filename, std::ios::binary};
    if (!in) {
        fmt::print(stderr, "error: {}: {}\n", filename, strerror(errno));
        exit(EXIT_FAILURE);
    }
    init(in);
}

Source::Source(const std::string &text) : filename("(none)") {
    std::stringstream ss{text};
    init(ss);
}

void Source::init(std::istream &in) {
    std::string line;
    while (std::getline(in, line)) {
        line_off.push_back(buf.size());
        buf.insert(buf.cend(), line.cbegin(), line.cend());
        buf.push_back('\n');
    }
    // Zero-terminate 'buf'.  This eases EOS handling in the lexer.
    buf.push_back('\0');
}

// TODO: perf shows this as the main bottleneck.
SourceLoc Source::locate(size_t pos) const {
    // Search linearly for the current line.
    int line;
    for (line = 0; static_cast<size_t>(line) < line_off.size(); line++) {
        if (pos < line_off[line])
            break;
    }
    int col = pos - line_off[line - 1] + 1;
    return SourceLoc{filename, line, col};
}

} // namespace cmp
