ocamlc -c sm5.mli
ocamlc -c sm5.ml
ocamlc -c sonata.mli
ocamlc -c sonata.ml
ocamlc -c parser.ml
ocamlc -c rozettax.ml
ocamlc -c main.ml
ocamlc -o run.exe sm5.cmo str.cma parser.cmo sonata.cmo rozettax.cmo main.cmo
