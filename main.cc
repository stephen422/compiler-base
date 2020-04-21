#include "driver.h"

using namespace cmp;

int main(int argc, char **argv) {
    if (argc < 2) {
        fmt::print(stderr, "error: no filename specified\n");
        return 1;
    }

    // XXX: We don't even need to declare Driver variables, why not make these
    // free functions?
    auto d1 = Driver::from_path(Path{"../test_parser.txt"});
    d1.compile();
    d1.verify();
    auto d2 = Driver::from_path(Path{"../test_namebind.txt"});
    d2.compile();
    d2.verify();
    auto d3 = Driver::from_path(Path{"../test_typeck.txt"});
    d3.compile();
    d3.verify();

    return EXIT_SUCCESS;
}
