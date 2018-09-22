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

Source::Source(const std::string &p) : path(p)
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
        std::cout << "======== " << line_off[line_off.size() - 1] << std::endl;
    }
}
