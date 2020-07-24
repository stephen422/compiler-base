compiler-base
=============

Hobby compiler project for a simple systems programming language.

Highlights
----------

* Memory safety using a borrow checker that resembles that of Rust.  Check out
  [test_borrowck.txt](test_borrowck.txt) to see what it can do.

Building
--------

The project uses CMake and requires a compiler with C++17 support.

    $ mkdir build
    $ cmake ..
    $ make

To run unit tests, do:

    $ make check

Look into `test_*.txt` files for some examples.

(C) 2020 Hansung Kim
