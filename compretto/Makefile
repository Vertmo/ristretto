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

parseLex: src/frontend/lexer.mll src/frontend/parser.mly
	mkdir -p src/frontend/_parselexgen
	ocamllex -o src/frontend/_parselexgen/lexer.ml src/frontend/lexer.mll
	ocamlyacc -b src/frontend/_parselexgen/parser src/frontend/parser.mly

compile: parseLex
	ocamlbuild compretto.native -Is src/,src/frontend,src/frontend/_parselexgen,src/frontend/ast

clean:
	rm -r src/frontend/_parselexgen
	ocamlbuild -clean
