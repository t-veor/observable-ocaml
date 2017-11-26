.PHONY: all clean byte native sanity

OCB_FLAGS = -pkg compiler-libs -lib ocamlcommon -I src
OCB = ocamlbuild $(OCB_FLAGS)

all : native byte

clean :
	$(OCB) -clean

native : sanity
	$(OCB) main.native

byte : sanity
	$(OCB) main.byte

debug : sanity
	$(OCB) -tag debug main.byte

sanity :
	ocamlfind query compiler-libs
