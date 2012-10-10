all:
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

check:
	for package in $(PMFILES); do \
		perl -c $$package; \
	done
