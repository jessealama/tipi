## C compiler configuration for sb-grovel and cffi-grovel.
CC := gcc

## Preferred Common Lisp implementation.
# Choices are SBCL, CMUCL, CCL, ECL, ACL (Allegro), CLISP and ABCL.
LISP := CCL

## Implementation paths. Note that regardless of the above setting, SBCL is
# required in order to generate the reference manual, so you must set
# SBCL_PATH properly. If you don't have SBCL installed at all, set it to
# nothing.
SBCL_PATH  := CC=$(CC) sbcl
CCL_PATH   := ccl

## Programs for generating the documentation:

ASDF_FILE := tipi.asd

SBCL_CACHE  := sbcl
SBCL_BINLOC := sbcl
SBCL_LOAD   := --load
SBCL_EVAL   := --eval
SBCL_DUMP   := --script

CCL_CACHE  := ccl
CCL_BINLOC := openmcl
CCL_LOAD   := --load
CCL_EVAL   := --eval
CCL_DUMP   := --no-init $(CCL_LOAD)

$(DEMOS): %: %.lisp
	$($(LISP)_PATH) $(EVAL_CONFIG) $($(LISP)_DUMP) $<

tipi: %: %.lisp
	which $($(LISP)_PATH)
	$($(LISP)_PATH) $(EVAL_CONFIG) $($(LISP)_DUMP) $<

all: tipi
	make -C xsl

check:
	make -C lib check
