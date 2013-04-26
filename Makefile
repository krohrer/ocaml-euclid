top:
	ocamlbuild -use-ocamlfind -pkg oUnit ocaml-euclid.top

run:
	ocamlbuild -use-ocmalfind -pkg oUnit euclid-test.native && ./euclid-test.native
