MODULER=ocamlopt

test:
	echo "Test Excercise 4"
	@$(MODULER) -c ex4_4.ml
	@$(MODULER) -c testlib.ml
	@$(MODULER) ex4_4.cmx testlib.cmx test4_4.ml -o test4_4.out
	./test4_4.out
	echo "Test Excercise 5"
	@$(MODULER) -c ex4_5.ml
	@$(MODULER) -c testlib.ml
	@$(MODULER) ex4_5.cmx testlib.cmx test4_5.ml -o test4_5.out
	./test4_5.out

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo *.out