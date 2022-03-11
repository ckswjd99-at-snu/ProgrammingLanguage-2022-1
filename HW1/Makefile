RUNTIME=ocaml
COMPILER=ocamlc
MODULER=ocamlopt

all: test

clean: 
	rm -f *.o *.cmi *.cmx *.cmo

modules:
	ocamlopt -c ex1_1.ml
	ocamlopt -c ex1_2.ml
	ocamlopt -c ex1_3.ml
	ocamlopt -c ex1_4.ml
	ocamlopt -c ex1_5_new.ml
	ocamlopt -c ex1_6.ml