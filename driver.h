// -*- C++ -*-
#ifndef CMP_DRIVER_H
#define CMP_DRIVER_H

#include "source.h"
#include "error.h"

using namespace cmp;

struct Driver {
  const Source source;
  std::vector<Error> errors;
  std::vector<Error> beacons;

  // Construct from a filepath.
  Driver(const Path &path) : source{path} {}
  // Construct from a string text.
  Driver(const std::string &text) : source{text} {};
  static Driver from_path(const Path &path) { return Driver{path}; }
  static Driver from_text(const std::string &text) { return Driver{text}; }

  bool compile();
  void report() const;
  bool verify();
  bool no_errors() const { return errors.empty(); }
};

#endif
