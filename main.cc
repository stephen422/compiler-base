#include "driver.h"

using namespace cmp;

int main(int argc, char **argv) {
    if (argc < 2) {
        fmt::print(stderr, "error: no filename specified\n");
        return 1;
    }

    // XXX: We don't even need to declare Driver variables, why not make these
    // free functions?
    auto d1 = Driver::from_path(Path{argv[1]});
    d1.compile();
    d1.report();

    return EXIT_SUCCESS;
}
