.PHONY: build test install uninstall reinstall clean

FINDLIB_NAME=unix-sys-stat
MOD_NAME=unix_sys_stat

OCAML_LIB_DIR=$(shell ocamlc -where)

CTYPES_LIB_DIR=$(shell ocamlfind query ctypes)

OCAMLBUILD=CTYPES_LIB_DIR=$(CTYPES_LIB_DIR) OCAML_LIB_DIR=$(OCAML_LIB_DIR) \
	ocamlbuild -use-ocamlfind -classic-display

WITH_UNIX=$(shell ocamlfind query ctypes unix > /dev/null 2>&1 ; echo $$?)

TARGETS=.cma .cmxa

PRODUCTS=$(addprefix sys_stat,$(TARGETS))

ifeq ($(WITH_UNIX), 0)
PRODUCTS+=$(addprefix $(MOD_NAME),$(TARGETS)) \
          lib$(MOD_NAME)_stubs.a dll$(MOD_NAME)_stubs.so \
          sys_stat_map.byte
endif

TYPES=.mli .cmi .cmti

INSTALL:=$(addprefix sys_stat,$(TYPES)) \
         $(addprefix sys_stat_host,$(TYPES)) \
         $(addprefix sys_stat,$(TARGETS))

INSTALL:=$(addprefix _build/lib/,$(INSTALL))

ifeq ($(WITH_UNIX), 0)
INSTALL_UNIX:=$(addprefix sys_stat_unix,$(TYPES)) \
              $(addprefix $(MOD_NAME),$(TARGETS))

INSTALL_UNIX:=$(addprefix _build/unix/,$(INSTALL_UNIX))

INSTALL+=$(INSTALL_UNIX)
endif

ARCHIVES:=_build/lib/sys_stat.a

ifeq ($(WITH_UNIX), 0)
ARCHIVES+=_build/unix/$(MOD_NAME).a
endif

build:
	$(OCAMLBUILD) $(PRODUCTS)

test: build
	$(OCAMLBUILD) unix_test/test.native
	./test.native

install:
	ocamlfind install $(FINDLIB_NAME) META \
		$(INSTALL) \
		-dll _build/unix/dll$(MOD_NAME)_stubs.so \
		-nodll _build/unix/lib$(MOD_NAME)_stubs.a \
		$(ARCHIVES)

uninstall:
	ocamlfind remove $(FINDLIB_NAME)

reinstall: uninstall install

clean:
	ocamlbuild -clean
	rm -f lib/sys_stat.cm? unix/sys_stat_unix.cm? \
	      lib/sys_stat.o unix/sys_stat_unix.o
