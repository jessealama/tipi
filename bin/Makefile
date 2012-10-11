.PHONY: all clean test

shell-scripts = reprove-semantically-summarize
shs = $(addsuffix .sh,$(shell-scripts))
perl-scripts = solvable tipi
pls = $(addsuffix .pl,$(perl-scripts))
editable-files = $(shs) $(pls) Makefile
emacs-backups = $(addsuffix ~,$(editable-files))

all: $(pls) $(shs)

%.sh:
	bash -n $*.sh

%.pl:
	perl -c $*.pl

clean:
	rm -f $(emacs-backups)

test: $(shs) $(pls)
	# no tests defined yet
