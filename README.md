# Ristretto #

Compiler for a small statically typed programming language, with syntax inspired from Rust and Javascript, and designed to be run on the JVM.

## Dependencies ##
* OCaml v4.06.1
* OCamlbuild
* OUnit2
* GNU Make

## Usage ##
There is a Makefile included at the root of the project, which does pretty much everything you could want :
* `make` compiles the compiler
* `make tests` compiles and run the unit tests
* `make samples` compiles all the code samples. The class produced can be found in the *tests/samples* directory. You can run them using the JVM v6 or later version.

## License ##
This project is licensed under the GNU General Public License v3.0. See [LICENSE](LICENSE) for more information.
