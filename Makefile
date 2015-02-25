
.PHONY: all clean

default: all

position.cmo:
	ocamlfind ocamlc -package facile -c position.ml

ex.cmo: position.cmo
	ocamlfind ocamlc -package facile,ocamlgraph -c ex.ml

all: ex.cmo

clean:
	rm -fr *.cmo
