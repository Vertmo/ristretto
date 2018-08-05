################################################################################
##                                                                            ##
##                Compiler for the Ristretto programming language             ##
##                                                                            ##
##                                Basile Pesin                                ##
##                                                                            ##
## Copyright 2018 Basile Pesin. This file is licensed under the terms of the  ##
##                    GNU General Public License v3.0                         ##
################################################################################

SRC = src/,src/frontend,src/frontend/_parselexgen,src/frontend/ast


all: compile

compile:
	ocamlbuild compretto.native -Is $(SRC)
clean:
	ocamlbuild -clean
