#ifndef ERROR_H
#define ERROR_H

#include "source.h"
#include <regex>

namespace cmp {

struct Error {
    SourceLoc loc;
    std::string message;

    Error(SourceLoc loc, const std::string &msg) : loc(loc), message(msg) {}
    std::string str() const;
};

bool verify(const std::string &filename, std::vector<Error> &errors,
            const std::vector<Error> &beacons);

} // namespace cmp

#endif
