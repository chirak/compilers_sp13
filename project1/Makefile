# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc
 
all: clean yacc

utility:
	$(COMPILER) -c explode.ml

com:
	$(COMPILER) -c ast.ml
	$(COMPILER) -c eval.ml

	
yacc: com
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c fishyacc.ml
	$(COMPILER) -o ps1yacc ast.cmo parse.cmo lex.cmo eval.cmo fishyacc.cmo
	cp ps1yacc test/

clean:
	-rm *.cmo *.cmi ps1yacc test/ps1yacc parse.ml parse.mli lex.ml
