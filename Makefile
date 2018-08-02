################################################################################
##                                                                            ##
##                Compiler for the Ristretto programming language             ##
##                                                                            ##
##                                Basile Pesin                                ##
##                                                                            ##
## Copyright 2018 Basile Pesin. This file is licensed under the terms of the  ##
##                    GNU General Public License v3.0                         ##
################################################################################

FOLDERS = compretto tests

compile:
	for folder in $(FOLDERS); do ($(MAKE) --no-print-directory -C $$folder compile); done

tests: compile
	$(MAKE) --no-print-directory -C tests tests

clean:
	for folder in $(FOLDERS); do ($(MAKE) --no-print-directory -C $$folder clean); done
