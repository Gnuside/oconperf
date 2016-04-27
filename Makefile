include $(shell ocamlc -where)/Makefile.config

PROGRAM=oconperf
LIB_PROGRAM=liboconperf

all: build_native build_byte build_lib

build_native:
	corebuild $(PROGRAM).native
	ln -sf $(PROGRAM).native $(PROGRAM)

build_byte:
	corebuild $(PROGRAM).byte

build_lib: build_native_lib build_byte_lib build_native_shared_lib

build_native_lib:
	corebuild $(LIB_PROGRAM).cmxa

build_native_shared_lib:
	corebuild $(LIB_PROGRAM).cmxs

build_byte_lib:
	corebuild $(LIB_PROGRAM).cma

profile:
	corebuild $(PROGRAM).p.native

docker:
	docker build -t oconperf-server .

clean:
	ocamlbuild -clean
	rm -f $(PROGRAM)

install: build_native build_byte
	ocamlfind install $(PROGRAM) META \
	  $(wildcard _build/src/*.byte) $(wildcard _build/src/*.native) \
	  $(wildcard _build/src/*.a) $(wildcard _build/src/*.mli)

install_lib: build_lib
	ocamlfind install $(PROGRAM) META \
	  $(wildcard _build/$(LIB_PROGRAM)/*.cm[xioat]) \
	  $(wildcard _build/$(LIB_PROGRAM)/*.cmx[as]) \
	  $(wildcard _build/$(LIB_PROGRAM)/*.mli) \
	  $(wildcard _build/$(LIB_PROGRAM)/*$(EXT_LIB))

uninstall:
	ocamlfind remove $(PROGRAM)
