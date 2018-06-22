#include <iostream>
#include <string>
#include <vector>

/// Source content handler for file reading, position reporting and so
/// on.
class Source {
public:
    // Create from a filepath.
    Source(const std::string &p);

private:
    const std::string path;
    std::vector<char> buf;
};
