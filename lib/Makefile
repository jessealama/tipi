.PHONY: all

all: check
	# nothing to do

PMFILES=$(wildcard *.pm)
editable-files = $(PMFILES) Makefile
emacs-backups = $(wildcard *~)

check:
	ok="1"; \
        for package in $(PMFILES); do perl -c $$package || ok="0"; done; \
        [ "$$ok" = "1" ];

clean:
	rm -f $(emacs-backups)
