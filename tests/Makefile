################################################################################
##                                                                            ##
##                Compiler for the Ristretto programming language             ##
##                                                                            ##
##                                Basile Pesin                                ##
##                                                                            ##
## Copyright 2018 Basile Pesin. This file is licensed under the terms of the  ##
##                    GNU General Public License v3.0                         ##
################################################################################

SRC = compretto/src,compretto/src/frontend,compretto/src/frontend/ast,compretto/src/middleend/analysis

compile:
	ocamlbuild -cflags -w,-8 -Is $(SRC) -package oUnit tests.native

tests: compile
	./tests.native -no-output-file -no-cache-filename

clean:
	ocamlbuild -clean
