.PHONY: all

all: check

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

check:
	ok="1"; \
        for package in $(PMFILES); do perl -c $$package || ok="0"; done; \
        [ "$$ok" = "1" ];
