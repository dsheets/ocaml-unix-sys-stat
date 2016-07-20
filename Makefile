.PHONY: build test install uninstall reinstall clean

FINDLIB_NAME=unix-sys-stat
MOD_NAME=unix_sys_stat

OCAML_LIB_DIR=$(shell ocamlc -where)
LWT_LIB_DIR=$(shell ocamlfind query lwt)
CTYPES_LIB_DIR=$(shell ocamlfind query ctypes)

OCAMLBUILD=CTYPES_LIB_DIR=$(CTYPES_LIB_DIR) \
           LWT_LIB_DIR=$(LWT_LIB_DIR)       \
           OCAML_LIB_DIR=$(OCAML_LIB_DIR)   \
	ocamlbuild -use-ocamlfind -classic-display -plugin-tags 'package(ctypes-build.ocamlbuild)'

WITH_UNIX=$(shell ocamlfind query ctypes unix > /dev/null 2>&1 ; echo $$?)
WITH_LWT=$(shell ocamlfind query lwt > /dev/null 2>&1 ; echo $$?)

TARGETS=.cma .cmxa

PRODUCTS=$(addprefix sys_stat,$(TARGETS))

ifeq ($(WITH_UNIX), 0)
PRODUCTS+=$(addprefix $(MOD_NAME),$(TARGETS)) \
          lib$(MOD_NAME)_stubs.a dll$(MOD_NAME)_stubs.so \
          sys_stat_map.byte
endif

ifeq ($(WITH_LWT), 0)
PRODUCTS+=$(addprefix $(MOD_NAME)_lwt,$(TARGETS)) \
          lib$(MOD_NAME)_lwt_stubs.a dll$(MOD_NAME)_lwt_stubs.so
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

ifeq ($(WITH_LWT), 0)
INSTALL_LWT:=$(addprefix sys_stat_unix_lwt,$(TYPES)) \
             $(addprefix $(MOD_NAME)_lwt,$(TARGETS))

INSTALL_LWT:=$(addprefix _build/lwt/,$(INSTALL_LWT))
INSTALL_LWT:=$(INSTALL_LWT) \
	      -dll _build/lwt/dll$(MOD_NAME)_lwt_stubs.so \
	      -nodll _build/lwt/lib$(MOD_NAME)_lwt_stubs.a

INSTALL+=$(INSTALL_LWT)
endif

ARCHIVES:=_build/lib/sys_stat.a

ifeq ($(WITH_UNIX), 0)
ARCHIVES+=_build/unix/$(MOD_NAME).a
endif

ifeq ($(WITH_LWT), 0)
ARCHIVES+=_build/lwt/$(MOD_NAME)_lwt.a
endif

build:
	$(OCAMLBUILD) $(PRODUCTS)

test: build
	$(OCAMLBUILD) lib_test/test.native
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
