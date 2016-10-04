all:
	ocamlbuild -use-ocamlfind -r -cflag -g -pkgs ssl,lwt.unix,lwt,sqlite3,yojson main.native
location:
	ocamlbuild -use-ocamlfind -r -pkgs yojson location.native
clean:
	ocamlbuild -clean
