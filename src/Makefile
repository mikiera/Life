test:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal test_main.byte && ./test_main.byte

play:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal main.byte && ./main.byte 

check:
	bash checkenv.sh && bash checktypes.sh 

clean:
	ocamlbuild -clean
	
zip:
	zip lifeof3110.zip *.ml{,i,y,l}