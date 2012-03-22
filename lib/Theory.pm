package Theory;

use Moose;
use IPC::Cmd qw(can_run);
use IPC::Run qw(run start timer harness);
use Carp qw(croak);
use Readonly;
use charnames qw(:full);
use List::MoreUtils qw(firstidx);
use File::Temp qw(tempfile);

use Formula;
use Utils qw(ensure_readable_file slurp);

Readonly my $TPTP4X => 'tptp4X';
Readonly my $EMPTY_STRING => q{};

has 'path' => (
    is => 'ro',
    isa => 'Str',
    reader => 'get_path',
    required => 1,
);

has 'conjecture' => (
    isa => 'Formula',
    is => 'rw',
    writer => '_set_conjecture',
);

has 'formula_table' => (
    isa => 'HashRef',
    is => 'rw',
    writer => '_set_formula_table',
    reader => 'get_formula_table',
);

sub BUILD {
    my $self = shift;

    my %formula_table = ();

    my @formulas = $self->get_formulas (1);

    foreach my $formula (@formulas) {
	my $name = $formula->get_name ();
	my $status = $formula->get_status ();
	$formula_table{$name} = $formula;
	if ($status eq 'conjecture') {
	    $self->_set_conjecture ($formula);
	}
    }

    $self->_set_formula_table (\%formula_table);

}

sub content {
    my $self = shift;
    my $path = $self->get_path ();
    return slurp ($path);
}

sub formula_with_name {
    my $self = shift;
    my $name = shift;

    my %formula_table = %{$self->get_formula_table ()};

    if (defined $formula_table{$name}) {
	return $formula_table{$name};
    } else {
	my $path = $self->get_path ();
	croak 'No such formula called \'', $name, '\' in the TPTP file ', $path, '.';
    }
}

sub get_formulas {
    my $self = shift;
    my $expand_includes = shift;

    my $path = $self->get_path ();

    my @tptp4x_call = ('tptp4X', '-N', '-V', '-c', '-umachine');

    if (defined $expand_includes && $expand_includes) {
	push (@tptp4x_call, '-x');
    }

    push (@tptp4x_call, $path);

    my $tptp4x_err = $EMPTY_STRING;
    my $tptp4x_out = $EMPTY_STRING;

    my $tptp4x_harness;

    if (defined $expand_includes && $expand_includes) {
	$tptp4x_harness = harness (\@tptp4x_call,
				   '>', \$tptp4x_out,
				   '2>', \$tptp4x_err);
    } else {
	my @grep_call = ('grep', '--invert-match', '^include(');
	$tptp4x_harness = harness (\@tptp4x_call,
				   '|',
				   \@grep_call,
				   '>', \$tptp4x_out,
				   '2>', \$tptp4x_err);

    }

    $tptp4x_harness->start ();
    $tptp4x_harness->finish ();

    my $tptp4x_exit_code = $tptp4x_harness->result (0);

    if ($tptp4x_exit_code != 0) {
	if ($tptp4x_err eq $EMPTY_STRING) {
	    croak 'tptp4X did not exit cleanly working on ', $path, '; there was no error output.';
	} else {
	    croak 'tptp4X did not exit cleanly working on ', $path, '; the error output was:', "\n", $tptp4x_err;
	}
    }

    my @formula_strings = split ("\n", $tptp4x_out);

    my @formulas = map { Formula::make_formula ($_) } @formula_strings;

    if (wantarray) {
	return @formulas;
    } else {
	return \@formulas;
    }

}

sub get_conjecture {
    my $self = shift;
    my @formulas = $self->get_formulas ();
    my $conjecture = undef;
  CONJECTURE:
    foreach my $formula (@formulas) {
	my $status = $formula->get_status ();
	if ($status eq 'conjecture') {
	    $conjecture = $formula;
	    last CONJECTURE;
	}
    }

    if (defined $conjecture) {
	return $conjecture;
    } else {
	my $path = $self->get_path ();
	croak 'We did not find a conjecture formula in the TPTP file at ', $path, '.';
    }
}

sub get_axioms {
    my $self = shift;
    my $expand_includes = shift;

    my @formulas = $self->get_formulas ($expand_includes);

    my @axioms = ();
    foreach my $formula (@formulas) {
	my $status = $formula->get_status ();
	if ($status ne 'conjecture') {
	    push (@axioms, $formula);
	}
    }

    if (wantarray) {
	return @axioms;
    } else {
	return \@axioms;
    }

}

sub has_conjecture_formula {
    my $self = shift;
    my @formulas = $self->get_formulas (0); # don't expand
    my $conjecture_formula = undef;
    my $conjecture_idx = firstidx { $_->get_status () eq 'conjecture' } @formulas;

    return ($conjecture_idx >= 0);

}

sub strip_conjecture {
    my $self = shift;

    my $path = $self->get_path ();
    my @axioms = $self->get_axioms ();

    (my $new_fh, my $new_path) = tempfile ();

    foreach my $axiom (@axioms) {
	print {$new_fh} $axiom->tptpify (), "\N{LF}";
    }

    close $new_fh
	or croak 'Error: unable to close the output filehandle for the conjecture-free variant of ', $path, '.';

    return Theory->new (path => $new_path);

}

sub promote_conjecture_to_true_axiom {
    my $self = shift;

    my $path = $self->get_path ();
    my @axioms = $self->get_axioms ();

    (my $new_fh, my $new_path) = tempfile ();

    foreach my $axiom (@axioms) {
	print {$new_fh} $axiom->tptpify (), "\N{LF}";
    }

    if ($self->has_conjecture_formula ()) {
	my $conjecture = $self->get_conjecture ();
	my $conjecture_as_axiom = $conjecture->change_status ('axiom');
	print {$new_fh} $conjecture_as_axiom->tptpify (), "\N{LF}";
    }

    close $new_fh
	or croak 'Error: unable to close the output filehandle for the conjecture-free variant of ', $path, '.';

    return Theory->new (path => $new_path);
}

sub promote_conjecture_to_false_axiom {
    my $self = shift;

    my $path = $self->get_path ();
    my @axioms = $self->get_axioms ();

    (my $new_fh, my $new_path) = tempfile ();

    foreach my $axiom (@axioms) {
	print {$new_fh} $axiom->tptpify (), "\N{LF}";
    }

    if ($self->has_conjecture_formula ()) {
	my $conjecture = $self->get_conjecture ();
	my $negated_conjecture = $conjecture->negate ();
	my $conjecture_as_axiom = $negated_conjecture->change_status ('axiom');
	print {$new_fh} $conjecture_as_axiom->tptpify (), "\N{LF}";
    }

    close $new_fh
	or croak 'Error: unable to close the output filehandle for the conjecture-free variant of ', $path, '.';

    return Theory->new (path => $new_path);
}

sub remove_formula {
    my $self = shift;
    my $formula_to_remove = shift;

    my $name_of_formula_to_remove = $formula_to_remove->get_name ();

    my $path = $self->get_path ();
    my @axioms = $self->get_axioms ();

    (my $new_fh, my $new_path) = tempfile ();

    foreach my $axiom (@axioms) {
	my $axiom_name = $axiom->get_name ();
	if ($axiom_name ne $name_of_formula_to_remove) {
	    print {$new_fh} $axiom->tptpify (), "\N{LF}";
	}
    }

    if ($self->has_conjecture_formula ()) {
	my $conjecture = $self->get_conjecture ();
	print {$new_fh} $conjecture->tptpify (), "\N{LF}";
    }

    close $new_fh
	or croak 'Error: unable to close the output filehandle for the conjecture-free variant of ', $path, '.';

    return Theory->new (path => $new_path);

}

sub add_formula {
    my $self = shift;
    my $formula = shift;

    my $path = $self->get_path ();
    my @axioms = $self->get_axioms (1);

    (my $new_fh, my $new_path) = tempfile ();

    foreach my $axiom (@axioms) {
	my $axiom_name = $axiom->get_name ();
	print {$new_fh} $axiom->tptpify (), "\N{LF}";
    }

    print {$new_fh} $formula->tptpify (), "\N{LF}";

    close $new_fh
	or croak 'Error: unable to close the output filehandle for ', $path, '.';

    return Theory->new (path => $new_path);

}

1;
__END__
