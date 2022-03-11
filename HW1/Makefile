RUNTIME=ocaml
COMPILER=ocamlc
MODULER=ocamlopt

clean: 
	@rm -f *.o *.out *.cmi *.cmx *.cmo test

modules:
	@$(MODULER) -c ex1_1.ml
	@$(MODULER) -c ex1_2.ml
	@$(MODULER) -c ex1_3.ml
	@$(MODULER) -c ex1_4.ml
	@$(MODULER) -c ex1_5.ml
	@$(MODULER) -c ex1_6.ml

compile_test:
	$(COMPILER) ex1_1.ml
	$(COMPILER) ex1_2.ml
	$(COMPILER) ex1_3.ml
	$(COMPILER) ex1_4.ml
	$(COMPILER) ex1_5.ml
	$(COMPILER) ex1_6.ml

test: clean modules
	@$(MODULER) ex1_1.cmx test1_1.ml -o test1_1.out
	@$(MODULER) ex1_2.cmx test1_2.ml -o test1_2.out
	@$(MODULER) ex1_3.cmx test1_3.ml -o test1_3.out
	@$(MODULER) ex1_4.cmx test1_4.ml -o test1_4.out
	@$(MODULER) ex1_5.cmx test1_5.ml -o test1_5.out
	@$(MODULER) ex1_6.cmx test1_6.ml -o test1_6.out
	@./test1_1.out
	@./test1_2.out
	@./test1_3.out
	@./test1_4.out
	@./test1_5.out
	@./test1_6.out
