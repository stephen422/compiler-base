#include "error.h"

namespace cmp {

std::string Error::toString() const {
    return fmt::format("{}: error: {}", loc.toString(), message);
}

// Verify errors against the error beacons embedded in the source text.
bool verify(const std::string &filename, const std::vector<Error> &errors,
            const std::vector<Error> &beacons) {
    bool success = true;
    fmt::print("\033[0;32mtest\033[0m {}:\n", filename);

    size_t i = 0, j = 0;
    while (i < errors.size() && j < beacons.size()) {
        auto error = errors[i];
        auto beacon = beacons[j];
        if (error.loc.line == beacon.loc.line) {
            std::string stripped{std::cbegin(beacon.message) + 1,
                                 std::cend(beacon.message) - 1};
            std::regex regex{stripped};
            if (!std::regex_search(error.message, regex)) {
                success = false;
                fmt::print("< {}\n> {}\n", error.toString(), beacon.toString());
            }
            i++;
            j++;
        } else if (error.loc.line < beacon.loc.line) {
            success = false;
            fmt::print("< {}\n", error.toString());
            i++;
        } else {
            success = false;
            fmt::print("> {}\n", beacon.toString());
            j++;
        }
    }
    for (; i < errors.size(); i++) {
        success = false;
        fmt::print("< {}\n", errors[i].toString());
    }
    for (; j < beacons.size(); j++) {
        success = false;
        fmt::print("> {}\n", beacons[j].toString());
    }

    fmt::print("{} {}\n",
               success ? "\033[0;32msuccess\033[0m" : "\033[0;31mfail\033[0m",
               filename);
    return success;
}

} // namespace cmp
