#include "error.h"
#include <algorithm>
#include "fmt/format.h"

namespace cmp {

std::string Error::str() const {
    return fmt::format("{}: error: {}", loc.str(), message);
}

// Verify errors against the error beacons embedded in the source text.
bool verify(const std::string &filename, std::vector<Error> &errors,
            const std::vector<Error> &beacons) {
  bool success = true;
  // fmt::print("\033[0;32mtest\033[0m {}:\n", filename);

  // FIXME: Make sure the errors are shorted in ascending order in terms of
  // line numbers.
  std::sort(errors.begin(), errors.end(), [](const auto &e1, const auto &e2) {
    return e1.loc.line < e2.loc.line ||
           (e1.loc.line == e2.loc.line && e1.loc.col < e2.loc.col);
  });

  size_t i = 0, j = 0;
  while (i < errors.size() && j < beacons.size()) {
    auto error = errors[i];
    auto beacon = beacons[j];
    if (error.loc.line == beacon.loc.line) {
      std::regex regex{beacon.message};
      if (!std::regex_search(error.message, regex)) {
        success = false;
        printf("%s\n(expect) %s\n", error.str().c_str(), beacon.str().c_str());
      }
      i++;
      j++;
    } else if (error.loc.line < beacon.loc.line) {
      success = false;
      printf("%s\n", error.str().c_str());
      i++;
    } else {
      success = false;
      printf("(expect) %s\n", beacon.str().c_str());
      j++;
    }
  }
  for (; i < errors.size(); i++) {
    success = false;
    printf("%s\n", errors[i].str().c_str());
  }
  for (; j < beacons.size(); j++) {
    success = false;
    printf("(expect) %s\n", beacons[j].str().c_str());
  }

  // fmt::print("{} {}\n",
  //            success ? "\033[0;32msuccess\033[0m" : "\033[0;31mfail\033[0m",
  //            filename);
  if (!success) {
    printf("%s %s\n", "\033[0;31mfail\033[0m", filename.c_str());
  }

  return success;
}

} // namespace cmp
