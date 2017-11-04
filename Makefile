MAIN       = Main.native
#MAIN       = Main.d.byte
INTERFACES = $(wildcard *.mli)
MLS        = $(wildcard *.ml)
BUILD      = Makefile

UNAME_S   := $(shell uname -s)
UNAME_M   := $(shell uname -m)

LIBNAME    = jit_exec

ifeq ($(UNAME_S),Darwin)
OS         = macos
SO_SUFFIX  = dylib
LIB        = lib$(LIBNAME).$(SO_SUFFIX)
SHARED     = -dynamiclib -install_name $(PWD)/lib$(LIBNAME).$(SO_SUFFIX) \
             -arch x86_64 -compatibility_version 1 -current_version 1
OLFLAGS    = -lflags '-cclib,-L$(PWD),-cclib,-l$(LIBNAME)'
else ifeq ($(UNAME_S),Linux)
OS         = linux
SO_SUFFIX  = so
LIB        = lib$(LIBNAME).$(SO_SUFFIX)
SHARED     = -shared
OLFLAGS    = -lflags '-cclib $(PWD)/$(LIB)'
else
$(error Unsupported OS - only Linux and macOS are supported)
endif

ifneq ($(UNAME_M),x86_64)
$(error Unsupported architecture - only 64-bit Intel (x86_64) is supported)
endif

LIB_SRCS   = jit_exec.c jit_exec_fn_$(OS).s

TEST_ARGS  = '=6<4-j0sj0.' 2 2000000000

OCAMLBUILD = ocamlbuild
OPTS       = -use-ocamlfind \
	-pkg core \
	-pkg z3 \
	-pkg threads \
	-pkg ppx_deriving.std \
	-pkg ctypes \
	-pkg ctypes.foreign \
	-tag thread \
	-tag debug \
	-tag bin_annot \
	-tag short_paths \
	-cflags "-w A-4-33-40-41-42-43-34-44-27"  \
	-cflags -strict-sequence \
	-cflags -annot \
	-cflags -g \
	-no-hygiene \
	-lflags -g $(OLFLAGS)


# quiet output, but allow us to look at what commands are being
# executed by passing 'V=1' to make, without requiring temporarily
# editing the Makefile.
ifneq ($V, 1)
MAKEFLAGS += -s
endif

# GNU make, you are the worst.
.SUFFIXES:
%: %,v
%: RCS/%,v
%: RCS/%
%: s.%
%: SCCS/s.%

# list only those we use
.SUFFIXES: .ml .mli .d.byte .tex .pdf

all: test

setup:
	opam install core ppx_deriving ctypes ctypes-foreign ocaml-z3

%.d.byte: %.ml $(INTERFACES) $(MLS) $(BUILD) $(LIB)
	@echo "  OCAML $@"
	$(OCAMLBUILD) $(OPTS) $@
	touch -c $@

%.native: %.ml $(INTERFACES) $(MLS) $(BUILD) $(LIB)
	@echo "  OCAML $@"
	$(OCAMLBUILD) $(OPTS) $@
	touch -c $@

$(LIB): $(LIB_SRCS) $(BUILD)
	@echo "  CC -shared $@"
	cc -g -fPIC -Wall $(SHARED) -o $(LIB) $(LIB_SRCS)

clean:
	$(OCAMLBUILD) -clean
	rm -rf $(LIB)*

test: $(MAIN)
	@echo "  TEST ./Main.native $(TEST_ARGS)"
	./Main.native $(TEST_ARGS)

.PHONY: all clean test setup
