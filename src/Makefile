main:
	ocamlbuild -pkgs oUnit main.byte
	
test:
	ocamlbuild -pkgs oUnit,str,unix test.byte && ./test.byte

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean
	
zip:
	zip lifeof3110.zip *.ml{,i,y,l}