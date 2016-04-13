
PROGRAM=oconperf

all: build_native build_byte

clean:
	ocamlbuild -clean
	rm -f $(PROGRAM)

build_native:
	ocamlbuild -use-ocamlfind $(PROGRAM).native
	ln -sf $(PROGRAM).native $(PROGRAM)

build_byte:
	ocamlbuild -use-ocamlfind $(PROGRAM).byte

install: build_native build_byte
	ocamlfind install $(PROGRAM) META $(wildcard _build/*.cm[xioa]) $(wildcard _build/*.cmxa) $(wildcard *.o) $(wildcard _build/*.a) $(wildcard *.ml*)
