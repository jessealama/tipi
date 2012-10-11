.PHONY: all clean test check

shell-scripts = reprove-semantically-summarize
shs = $(addsuffix .sh,$(shell-scripts))
perl-scripts = solvable tipi
pls = $(addsuffix .pl,$(perl-scripts))
editable-files = $(shs) $(pls) Makefile
emacs-backups = $(addsuffix ~,$(editable-files))

all: check

clean:
	rm -f $(emacs-backups)

test:
	# no tests defined yet

check:
	ok="1"; \
        for script in $(shs); do bash -n $$script || ok="0"; done; \
        for script in $(pls); do perl -c $$script || ok="0"; done; \
        [ "$$ok" = "1" ];
