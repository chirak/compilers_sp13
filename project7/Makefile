# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc
 
all: clean cish cfg test

mips:
	$(COMPILER) -c word32.ml
	$(COMPILER) -c mips.ml

cish: mips
	$(COMPILER) -c cish_ast.ml
	ocamlyacc cish_parse.mly
	$(COMPILER) -c cish_parse.mli
	$(COMPILER) -c cish_parse.ml
	ocamllex cish_lex.mll
	$(COMPILER) -c cish_lex.ml
	$(COMPILER) -c cish_eval.ml
	$(COMPILER) -c cish_compile.ml

cfg: mips cish
	$(COMPILER) -c cish_ast.ml
	$(COMPILER) -c cfg_ast.ml
	$(COMPILER) -c cfg.ml
	$(COMPILER) -c interfere_graph.ml
	$(COMPILER) -c reg_alloc.ml
	$(COMPILER) -c main.ml
	$(COMPILER) -o run cish_ast.cmo cish_lex.cmo cish_parse.cmo word32.cmo mips.cmo cfg_ast.cmo cfg.cmo interfere_graph.cmo reg_alloc.cmo main.cmo

test: mips cish
	$(COMPILER) -c test_blocks.ml
	$(COMPILER) -c interfere_test.ml
	$(COMPILER) -c reg_alloc_test.ml
	$(COMPILER) -o run_test cish_ast.cmo cish_lex.cmo cish_parse.cmo word32.cmo mips.cmo cfg_ast.cmo cfg.cmo interfere_graph.cmo reg_alloc.cmo test_blocks.cmo interfere_test.ml reg_alloc_test.cmo

clean:
	-rm *.cmo *.cmi run_test run cish_parse.ml cish_parse.mli cish_lex.ml 
