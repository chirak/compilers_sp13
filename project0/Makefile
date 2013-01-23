# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc
 
all: clean mips

test: clean buildtest

mips:
	$(COMPILER) -c mips_ast.ml
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c byte.ml
	$(COMPILER) -c mach.ml
	$(COMPILER) -c mips_sim.ml
	$(COMPILER) -c mips.ml
	$(COMPILER) -o ps0 mips_ast.cmo parse.cmo lex.cmo byte.cmo mach.cmo mips_sim.cmo mips.cmo

buildtest:
	ocamlfind $(COMPILER) -o test -package oUnit -linkpkg -g mips_ast.ml byte.ml mach.ml mips_sim.ml mach_test.ml
clean:
	-rm *.cmo *.cmi test ps0 parse.ml parse.mli lex.ml 
