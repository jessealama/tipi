.PHONY: all

all: check
	# nothing to do

PACKAGES=CategoricalCommand \
	Command \
	ConjectureCommand \
	CountermodelCommand \
	Derivation \
	EproverDerivation \
	Formula \
	IndependenceCommand \
	Interpretation \
	Mace4Interpretation \
	MinimizeCommand \
	ModelCommand \
	NeededCommand \
	ParadoxInterpretation \
	PremisesCommand \
	ProveCommand \
	Prover9Derivation \
	ReproveCommand \
	Result \
	SZS \
	SelfcheckCommand \
	SymbolsCommand \
	TPTP \
	Theory \
	UsedPremisesCommand \
	Utils \
	VampireDerivation \
	VegasCommand \

PMFILES=$(addsuffix .pm,$(PACKAGES))
editable-files = $(PMFILES) Makefile
emacs-backups = $(addsuffix ~,$(editable-files))

check:
	ok="1"; \
        for package in $(PMFILES); do perl -c $$package || ok="0"; done; \
        [ "$$ok" = "1" ];

clean:
	rm -f $(emacs-backups)
