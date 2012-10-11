.PHONY: all check test clean

perl-tests = 00.load      \
             perlcritic   \
             pod-coverage \
             pod          \
             szs
ts = $(addsuffix .t,$(perl-tests))
editable-files = $(ts) Makefile
emacs-backups = $(addsuffix ~,$(editable-files))

all: check

clean:
	rm -f $(emacs-backups)

check:
	ok="1"; \
        for script in $(ts); do perl -c $$script || ok="0"; done; \
        [ "$$ok" = "1" ];

test:
	# we should probably run perl's prove command here
