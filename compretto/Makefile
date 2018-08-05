################################################################################
##                                                                            ##
##                Compiler for the Ristretto programming language             ##
##                                                                            ##
##                                Basile Pesin                                ##
##                                                                            ##
## Copyright 2018 Basile Pesin. This file is licensed under the terms of the  ##
##                    GNU General Public License v3.0                         ##
################################################################################

all: compile

compile:
	ocamlbuild compretto.native -Is src/,src/frontend,src/frontend/_parselexgen,src/frontend/ast

clean:
	ocamlbuild -clean
