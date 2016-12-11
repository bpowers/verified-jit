MAIN       = Main.native
#MAIN       = Main.d.byte
INTERFACES = $(wildcard *.mli)
MLS        = $(wildcard *.ml)
BUILD      = Makefile

UNAME_S   := $(shell uname -s)
UNAME_M   := $(shell uname -m)

ifeq ($(UNAME_S),Darwin)
SO_SUFFIX  = dylib
SHARED     = -dynamiclib -arch x86_64 -compatibility_version 1 -current_version 1
else ifeq ($(UNAME_S),Linux)
SO_SUFFIX  = so
SHARED     = -shared
else
$(error Unsupported OS - only Linux and macOS are supported)
endif

ifneq ($(UNAME_M),x86_64)
$(error Unsupported architecture - only 64-bit Intel (x86_64) is supported)
endif

LIB        = libjit_exec.$(SO_SUFFIX)

OCAMLBUILD = ocamlbuild
OPTS       = -use-ocamlfind \
	-pkg core \
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
	-lflags -g,-cclib,-l$(PWD)/$(LIB)


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
	opam install core ppx_deriving ctypes ctypes-foreign

%.d.byte: %.ml $(INTERFACES) $(MLS) $(BUILD) $(LIB)
	@echo "  OCAML $@"
	$(OCAMLBUILD) $(OPTS) $@
	touch -c $@

%.native: %.ml $(INTERFACES) $(MLS) $(BUILD) $(LIB)
	@echo "  OCAML $@"
	$(OCAMLBUILD) $(OPTS) $@
	touch -c $@

$(LIB): jit_exec.c jit_exec_fn.s $(BUILD)
	@echo "  CC -shared $@"
	cc -g -fPIC -Wall $(SHARED) -o $(LIB) jit_exec.c jit_exec_fn.s

clean:
	$(OCAMLBUILD) -clean
	rm -f *.out *.log *.aux $(LIB)

test: $(MAIN)
	@echo "  TEST"
	./Main.native '=6<4-j0sj0.' 2 20000000
#	LD_PRELOAD="$(PWD)/$(LIB)" ./Main.native '=6<4-j0sj0.' 2 20000000

.PHONY: all clean test setup
