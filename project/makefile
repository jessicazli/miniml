all: evaluation evaluation_tests expr expr_tests miniml

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

evaluation_tests: evaluation_tests.ml
	ocamlbuild -use-ocamlfind evaluation_tests.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

expr_tests: expr_tests.ml
	ocamlbuild -use-ocamlfind expr_tests.byte

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

clean:
	rm -rf _build *.byte