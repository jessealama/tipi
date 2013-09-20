.PHONY: all check test clean

emacs-backups = $(strip $(wildcard *~) $(wildcard .*~))

all:
	# nothing to do

check:
	# nothing to do

test:
	# nothing to do

clean:
ifneq ("$(emacs-backups)","")
	rm -f $(emacs-backups)
endif
