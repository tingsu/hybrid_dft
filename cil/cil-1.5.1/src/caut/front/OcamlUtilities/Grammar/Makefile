grammar: GrammarLexer.cmx Grammar.cmx GrammarMain.cmx
	ocamlopt -o grammar str.cmxa GrammarTypes.cmx GrammarLexer.cmx Grammar.cmx GrammarMain.cmx

GrammarTypes.cmx: GrammarTypes.ml
	ocamlopt -c GrammarTypes.ml

Grammar.cmx: GrammarTypes.cmx Grammar.mly
	ocamlyacc Grammar.mly
	ocamlopt -c Grammar.mli
	ocamlopt -c Grammar.ml

GrammarLexer.cmx: GrammarTypes.cmx Grammar.cmx GrammarLexer.mll
	ocamllex GrammarLexer.mll
	ocamlopt -c GrammarLexer.ml

GrammarMain.cmx: GrammarLexer.cmx Grammar.cmx GrammarMain.ml
	ocamlopt -c GrammarMain.ml

clean:
	rm -f Grammar.ml{,i} GrammarLexer.ml $(foreach file, Grammar GrammarLexer GrammarMain GrammarTypes, $(file).cmx $(file).cmi $(file).o)
