#ifndef CMP_ERROR_H
#define CMP_ERROR_H

#include "source.h"

namespace cmp {

struct Error {
    SourceLoc loc;
    std::string message;

    Error() {}
    Error(SourceLoc loc, const std::string &msg) : loc(loc), message(msg) {}
};

} // namespace cmp

#endif
