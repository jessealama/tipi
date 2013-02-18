.PHONY: all check clean

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

lisp-files = consequence \
             formulas    \
             independent \
             minimal     \
             model       \
             needed      \
             packages    \
             parse       \
             result      \
             run         \
             solve       \
             szs         \
             terms       \
             tipi        \
             tptp        \
             utils       \
             xslt

lisps = $(addsuffix .lisp,$(lisp-files))
asdfs = $(ASDF_FILE)
makefiles = Makefile

editable-files = $(lisps) $(asdfs) .gitignore $(makefiles) README.mkdn
emacs-backups = $(addsuffix ~,$(editable-files))
ccl-fasls = $(addsuffix .dx64fsl,$(lisp-files))
fasls = $(addsuffix .fasl,$(lisp-files))

subdirs = bin examples lib reference t xsl

tipi: $(lisps)
	which $($(LISP)_PATH)
	$($(LISP)_PATH) $(EVAL_CONFIG) $($(LISP)_DUMP) $<

all:
	+for dir in $(subdirs); do make -C $$dir all; done
	+make tipi

check:
	+for dir in $(subdirs); do make -C $$dir check; done

clean:
	rm -f tipi
	rm -f $(emacs-backups)
	rm -f $(ccl-fasls)
	rm -f $(fasls)
	+for dir in $(subdirs); do make -C $$dir clean; done

test:
	# no tests defined yet
	for dir in $(subdirs); do make -C $$dir test; done
