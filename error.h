#ifndef ERROR_H
#define ERROR_H

#include "source.h"
#include <regex>

namespace cmp {

struct Error {
  SourceLoc loc;
  std::string message;

  Error(SourceLoc loc, const std::string &msg) : loc(loc), message(msg) {}
};

bool verify(const std::string &filename, const std::vector<Error> &errors,
            const std::vector<Error> &beacons);

} // namespace cmp

template <> struct fmt::formatter<cmp::Error> {
  constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const cmp::Error &e, FormatContext &ctx) {
    // TODO: differentiate "parse error:" from "error:"?
    return format_to(ctx.out(), "{}: error: {}", e.loc, e.message);
  }
};

#endif
