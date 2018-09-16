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
SAMPLES = int.ris float.ris string.ris bool.ris let.ris binOp.ris boolExpr.ris if.ris print.ris function.ris fibonnaci.ris lorem.ris unit.ris double.ris long.ris

compile:
	for folder in $(FOLDERS); do ($(MAKE) --no-print-directory -C $$folder compile); done

tests: samples
	$(MAKE) --no-print-directory -C tests tests

samples: compile
	cd tests/samples; ../../compretto/compretto.native $(SAMPLES)

clean:
	for folder in $(FOLDERS); do ($(MAKE) --no-print-directory -C $$folder clean); done
	rm tests/samples/*.class
