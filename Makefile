SOURCES := main.ml location.ml
OUT := main.native
#OUT := location.native

PKGS := ssl,lwt.unix,lwt,sqlite3,yojson
OFLAGS := -r -cflag -g 

$(OUT) : $(SOURCES)
	ocamlbuild $(OFLAGS) -use-ocamlfind -pkgs $(PKGS) $(OUT)

clean:
	ocamlbuild -clean

run : $(OUT)
	./$(OUT)
