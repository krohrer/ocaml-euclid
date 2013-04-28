top:
	ocamlbuild -use-ocamlfind ocamleuclid.top

run:
	ocamlbuild -use-ocamlfind euclid.native && ./euclid.native

test:
	ocamlbuild -use-ocamlfind -pkg oUnit euclidtest.native && ./euclidtest.native
