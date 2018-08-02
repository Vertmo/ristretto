################################################################################
##                                                                            ##
##                Compiler for the Ristretto programming language             ##
##                                                                            ##
##                                Basile Pesin                                ##
##                                                                            ##
## Copyright 2018 Basile Pesin. This file is licensed under the terms of the  ##
##                    GNU General Public License v3.0                         ##
################################################################################

compile:
	ocamlbuild tests.native -package oUnit

tests: compile
	./tests.native -no-output-file -no-cache-filename

clean:
	ocamlbuild -clean
