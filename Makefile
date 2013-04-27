top:
	ocamlbuild -use-ocamlfind -pkg oUnit ocamleuclid.top

run:
	ocamlbuild -use-ocmalfind -pkg oUnit euclid-test.native && ./euclid-test.native
