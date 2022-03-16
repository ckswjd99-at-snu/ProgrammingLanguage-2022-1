RUNTIME=ocaml
COMPILER=ocamlc
MODULER=ocamlopt

all: compile_test

clean: 
	@rm -f *.o *.out *.cmi *.cmx *.cmo test
	@rm -f testfile/*.o testfile/*.out testfile/*.cmi testfile/*.cmx testfile/*.cmo testfile/test

modules:
	@$(MODULER) -c ex2_1.ml
	@$(MODULER) -c ex2_2.ml
	@$(MODULER) -c ex2_3.ml
	@$(MODULER) -c ex2_4.ml

compile_test:
	$(COMPILER) ex2_1.ml
	$(COMPILER) ex2_2.ml
	$(COMPILER) ex2_3.ml
	$(COMPILER) ex2_4.ml

test: clean modules
	@$(MODULER) ex2_1.cmx testfile/test2_1.ml -o testfile/test2_1.out
	@$(MODULER) ex2_2.cmx testfile/test2_2.ml -o testfile/test2_2.out
	@$(MODULER) ex2_3.cmx testfile/test2_3.ml -o testfile/test2_3.out
	@$(MODULER) ex2_4.cmx testfile/test2_4.ml -o testfile/test2_4.out
	@testfile/test2_1.out
	@testfile/test2_2.out
	@testfile/test2_3.out
	@testfile/test2_4.out
	@make clean
