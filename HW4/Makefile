MODULER=ocamlopt

test:
	echo "Test Excercise 4"
	@$(MODULER) -c ex4_4.ml
	@$(MODULER) -c testlib.ml
	@$(MODULER) ex4_4.cmx testlib.cmx test4_4.ml -o test4_4.out
	./test4_4.out