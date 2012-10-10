package SelfcheckCommand;

require v5.10;
use feature 'say';

use Moose;
use Carp qw(croak carp);
use Pod::Find qw(pod_where);
use Pod::Usage;
use Readonly;
use Getopt::Long qw(GetOptionsFromArray :config gnu_compat);
use charnames qw(:full);
use English qw(-no_match_vars);
use Data::Dumper;
use Term::ANSIColor qw(colored);
use Regexp::DefaultFlags;
use IPC::Cmd qw(can_run);
use List::Util qw(max);

use TPTP qw(supported_provers);
use Utils qw(error_message);

extends 'Command';

Readonly my $DESCRIPTION => 'Can you even run tipi?';
Readonly my $EMPTY_STRING => q{};
Readonly my $SPACE => q{ };
Readonly my $TWO_SPACES => q{  };
Readonly my $ASTERISK => q{*};
Readonly my $FULL_STOP => q{.};
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $MAPS_TO => '==>';
Readonly my $LF => "\N{LF}";

my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;

sub BUILD {
    my $self = shift;
    $self->_set_description ($DESCRIPTION);
    return $self;
}

around 'execute' => sub {
    my $orig = shift;
    my $self = shift;
    my @arguments = @_;

    GetOptionsFromArray (
	\@arguments,
	'man' => \$opt_man,
	'verbose' => \$opt_verbose,
	'help|?' => \$opt_help,
    ) or pod2usage (
	-exitval => 2,
	-input => pod_where({-inc => 1}, __PACKAGE__));

    if ($opt_help) {
        pod2usage(
	    -exitval => 1,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if ($opt_man) {
        pod2usage(
            -exitstatus => 0,
            -verbose    => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
        );
    }

    # debug implies verbose
    if ($opt_debug) {
        $opt_verbose = 1;
    }

    if (scalar @arguments > 0) {
	pod2usage (
	    -msg => error_message ('The selcheck command should be run with no arguments.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    return $self->$orig (@arguments);

};

my %can_run = ();

sub summarize_availability_of_program {
    my $program = shift;
    my $offset = shift;

    if (! defined $offset) {
	$offset = 0;
    }

    my $summary = $program . ($SPACE x $offset) . $SPACE . $MAPS_TO . $SPACE;

    if ($can_run{$program}) {
	$summary .= colored ($can_run{$program}, $GOOD_COLOR);
    } else {
	$summary .= colored ('Not Found', $BAD_COLOR);
    }

    return $summary;

}

sub summarize_availability_of_programs {
    my @programs = @_;

    my $report = $EMPTY_STRING;

    my @program_lengths = map { length $_ } @programs;
    my $max_program_length = max @program_lengths;

    foreach my $program (@programs) {
	my $program_length = length $program;
	my $offset = $max_program_length - $program_length;
	my $summary = summarize_availability_of_program ($program, $offset);
	$report .= $TWO_SPACES . $ASTERISK . $SPACE . $summary . $LF;
    }

    return $report;
}

sub execute {
    my $self = shift;

    my @infrastructure_programs = ('xsltproc',
				   'tptp4X',
				   'tptp2X',
				   'GetSymbols');
    my @provers = supported_provers ();
    my @auxiliary_programs = ('epclextract',
			      'interpformat',
			      'prooftrans');

    foreach my $program (@infrastructure_programs, @provers, @auxiliary_programs) {
	my $path = can_run ($program);
	$can_run{$program} = defined $path ? $path : 0;
    }

    my $infrastructure_summary
	= summarize_availability_of_programs (@infrastructure_programs);
    my $prover_summary = summarize_availability_of_programs (@provers);
    my $auxiliary_summary = summarize_availability_of_programs (@auxiliary_programs);

    say 'If you are reading this, then we were at least able to load Tipi';
    say 'as a collection of Perl programs, so you ought to be all set';
    say 'regarding Tipi\'s Perl dependencies.';

    say $EMPTY_STRING;

    say $infrastructure_summary;

    say 'Here are the results of searching for the essential programs';
    say 'that Tipi uses behind the scenes.  If any are not found, Tipi will';
    say 'not work at all for any non-trivial task.';

    say $EMPTY_STRING;

    say 'Here are the results of searching for the various automated reasoning';
    say 'programs that Tipi supports.  It is not an error if some of these are';
    say 'unavailable.  If, however, none are available, then you are in bad shape';
    say 'because you will not be able to do any interesting automated reasoning tasks';

    say $EMPTY_STRING;

    say $prover_summary;

    say 'Finally, here are the results of searching for various auxiliary programs';
    say 'that are used to help prepare input to the automated reasoners or to';
    say 'interpret their output.  Whether you truly need any of these depends on';
    say 'which automated reasoners you wish to use with Tipi.';

    say $EMPTY_STRING;

    say $auxiliary_summary;

    return 1;


}

1;
__END__

=pod

=head1 NAME

tipi selfcheck

=head1 SYNOPSIS

tipi selfcheck

=head1 DESCRIPTION

This simple command will check whether you are able to run B<tipi> at
all.  If it doesn't even run, then you are probably missing some Perl
dependencies.  If it does run but you are missing some other non-Perl
dependencies, you will be warned in brigh red text that cannot be
missed.

=cut
