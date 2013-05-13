top:
	ocamlbuild -use-ocamlfind -pkg ounit ocamleuclid.top

run:
	ocamlbuild -use-ocamlfind -pkg ounit euclid.native && ./euclid.native

test:
	ocamlbuild -use-ocamlfind -pkg ounit euclidtest.native && ./euclidtest.native
