package LDV::Utils;

# The debug printing package for all ldv perl tools.

use strict;
use vars qw(@ISA @EXPORT_OK @EXPORT);
@EXPORT=qw(&vsay print_debug_warning print_debug_normal print_debug_info print_debug_debug print_debug_trace print_debug_all get_debug_level check_system_call watcher_cmd);
#@EXPORT_OK=qw(set_verbosity);
use base qw(Exporter);

use POSIX;

# Stream where debug messages will be printed.
my $debug_stream = \*STDOUT;

my $verbosity = 10;

# "English" debug levels
my %levels = (
	"QUIET"    => 0,
	"WARNING"  => 4,
	"NORMAL"   => 10,
	"INFO"     => 20,
	"DEBUG"    => 30,
	"TRACE"    => 40,
	"ALL"      => 100,
);

my %backlev = reverse %levels;

sub from_eng
{
	my $lvl = uc shift;
	return $levels{$lvl} if exists $levels{$lvl};
	Carp::confess "Incorrect debug level: $lvl" unless $lvl =~ /^[0-9]*$/;
	return $lvl;
}

# Check whether a user specified verbosity level is greater then the package
# standard one.
sub check_verbosity
{
	my $level = shift || $ENV{'LDV_DEBUG'};
	$level = from_eng($level);
	return ($level <= $verbosity);
}

# Set verbosity level according to the value supplied or evironment variable
sub set_verbosity
{
	my $level = shift || $ENV{'LDV_DEBUG'};
	$level = from_eng($level);
	$verbosity = $level;
}

my @instrument = ($0);
sub push_instrument
{
	push @instrument,@_;
}
sub pop_instrument
{
	pop @instrument;
}

# Say something only if the number supplied is not less than current verbosity
# If the message consists of multiple lines, prepend verbosity info to each.
sub vsay
{
	my $v = from_eng shift;
	local $,=' ';
	local $_;
	if ($v <= $verbosity) {
		my $instrument = $instrument[-1];
		my $level_string = $backlev{$v};
		my $prepend_string = "";
		$prepend_string .= "$instrument: " if defined $instrument;
		$prepend_string .= "$level_string: ";

		# Make a nicely-looking text: newline at the end, and each line prepended with tool and severity info
		my $to_out = join($,,@_);
		$to_out = "$to_out\n" unless $to_out=~/\n$/;
		my @lines = map{"$prepend_string$_\n"} split(/\n/,$to_out,-1);
		pop @lines; # Last line that corresponded to last \n is spurious.
		print $debug_stream join("",@lines);
	}
}

# Debug printing functions output some information in depend on the debug level.
# args: the only string to be printed with a small formatting.
# retn: nothing.
sub print_debug_warning
{
	vsay('WARNING', "$_[0].\n");
}
sub print_debug_normal
{
	vsay('NORMAL', "$_[0].\n");
}
sub print_debug_info
{
	vsay('INFO', "$_[0].\n");
}
sub print_debug_debug
{
	vsay('DEBUG', "$_[0].\n");
}
sub print_debug_trace
{
	vsay('TRACE', "$_[0].\n");
}
sub print_debug_all
{
	vsay('ALL', "$_[0].\n");
}

# Determine the debug level in depend on the passed arguments.
# args: (the tool to be debugged name; the LDV_DEBUG value; the tool debug value).
# retn: verbosity to be used (in english).
sub get_debug_level
{
	my $tool_debug_name = shift;

	return 0 unless ($tool_debug_name);

	push_instrument($tool_debug_name);

	# By default (in case when neither the LDV_DEBUG nor the tool debug
	# environment variables aren't specified) or when they are both 0 just
	# information on critical errors is printed.
	# Otherwise the tool debug environment variable is preferable.
	my $ldv_debug = shift;
	my $tool_debug = shift;

	if ($tool_debug)
	{
		set_verbosity($tool_debug);
		print_debug_debug("The debug level is set correspondingly to the tool debug environment variable value '$tool_debug'");
	}
	elsif ($ldv_debug)
	{
		set_verbosity($ldv_debug);
		print_debug_debug("The debug level is set correspondingly to the general LDV_DEBUG environment variable value '$ldv_debug'.");
	}
	
	return $backlev{$verbosity};
}

sub check_system_call
{
	# This is got almost directly from the Perl manual:
	# http://perldoc.perl.org/functions/system.html
	if ($? == -1)
	{
		print("Failed to execute: $!\n");
		return -1;
	}
	elsif ($? & 127)
	{
		printf("Child died with signal %d, %s coredump\n", ($? & 127), ($? & 128) ? 'with' : 'without');

		die("The process was interrupted with CTRL+C") if (($? & 127) == 2);

		return ($? & 127);
	}
	elsif ($? >> 8)
	{
		printf("Child exited with value %d\n", ($? >> 8));
		return ($? >> 8);
	}

	return 0;
}

# Invokes command for LDV watched 
my $ldv_watcher = undef;
sub watcher_cmd_readall
{
	push_instrument("watcher");
	$ldv_watcher ||= ($ENV{'LDV_WATCHER_HOME'} || $ENV{'LDV_HOME'} || $ENV{'DSCV_HOME'})."/watcher/ldv-watcher";
	# Call watcher for the next RCV command
	my @watcher_args = ($ldv_watcher,@_);
	vsay('DEBUG',"Called watcher: @watcher_args\n");
	my $WATCHER; open $WATCHER, "-|", @watcher_args or die "INTEGRATION ERROR: watcher failed ($!): @watcher_args";
	# Read one line.  If none is printed, the line will contain undef;
	local $_;
	my @lines = ();
	while (<$WATCHER>){
		chomp;
		vsay('TRACE',"Watcher says: $_\n") if $_;
		push @lines,$_;
	}
	chomp for @lines;
	close $WATCHER;	# We don't need anything else

	# Check return values
	my $rv = $?;
	my $retcode = $?>>8;
	vsay('DEBUG',"Watcher returns $retcode, waitpid: $rv\n");
	# Return code of 1 means failure.  Other codes mean useful stuff
	die "INTEGRATION ERROR: watcher failed with retcode $retcode" if $retcode == 1;

	pop_instrument("watcher");

	return ([@lines],$retcode);
}

sub watcher_cmd
{
	push_instrument("watcher");
	$ldv_watcher ||= ($ENV{'LDV_WATCHER_HOME'} || $ENV{'LDV_HOME'} || $ENV{'DSCV_HOME'})."/watcher/ldv-watcher";
	# Call watcher for the next RCV command
	my @watcher_args = ($ldv_watcher,@_);
	vsay('DEBUG',"Called watcher: @watcher_args\n");
	my $WATCHER; open $WATCHER, "-|", @watcher_args or die "INTEGRATION ERROR: watcher failed ($!): @watcher_args";
	# Read one line.  If none is printed, the line will contain undef;
	my $line = <$WATCHER>;
	vsay('TRACE',"Watcher says: $line\n") if defined $line;
	chomp $line if defined $line;
	close $WATCHER;	# We don't need anything else

	# Check return values
	my $rv = $?;
	my $retcode = $?>>8;
	vsay('DEBUG',"Watcher returns $retcode, waitpid: $rv\n");
	# Return code of 1 means failure.  Other codes mean useful stuff
	die "INTEGRATION ERROR: watcher failed with retcode $retcode" if $retcode == 1;

	pop_instrument("watcher");

	return ($line,$retcode);
}

# (number,callback,args) -- call watcher with args, read no more than number lines from stdout, and call callback on each of them.  Returns the actual number of lines read.
sub watcher_cmd_callback
{
	my $max_lines = shift;
	my $callback = shift;

	push_instrument("watcher");
	$ldv_watcher ||= ($ENV{'LDV_WATCHER_HOME'} || $ENV{'LDV_HOME'} || $ENV{'DSCV_HOME'})."/watcher/ldv-watcher";
	# Call watcher for the next RCV command
	my @watcher_args = ($ldv_watcher,@_);
	vsay('DEBUG',"Called watcher: @watcher_args\n");

	my $WATCHER; my $pid = open $WATCHER, "-|", @watcher_args or die "INTEGRATION ERROR: watcher failed ($!): @watcher_args";
	my $lines_read = 0;
	my $line = undef;
	while ($lines_read < $max_lines){
		# Read the next line
		vsay('TRACE',"Read $lines_read lines, need to read $max_lines, reading next line...\n");
		$line = <$WATCHER>;
		# If it's EOF, exit without calling a callback
		last unless defined $line;
		chomp $line;
		vsay('TRACE',"Watcher says: $line\n");

		# Call back, and it returns how many useful lines we've read
		my $rv = $callback->($line);
		$rv ||= 0;
		$lines_read += $rv;
		vsay('DEBUG',"After callback, read $lines_read useful lines, need to read $max_lines.\n");
		vsay('TRACE',"Left callback.\n");
	}
	if ($lines_read >= $max_lines) {
		vsay('DEBUG',"Killing watcher...\n");
		# don't give the bastard a chance!
		kill SIGKILL,$pid;
		# Don't check the return code: we may be killing a dead process (which is unsafe, but still...)
	}
	vsay('TRACE',"Closing read pipe...\n");
	close $WATCHER;	# We don't need anything else.  This will just drop stuff in a buffer of a dead process.
	vsay('TRACE',"Read pipe closed.\n");

	# Check return values
	my $rv = $?;
	my $retcode = $?>>8;
	vsay('DEBUG',"Watcher returns $retcode, waitpid: $rv\n");
	# Return code of 1 means failure.  Other codes mean useful stuff
	die "INTEGRATION ERROR: watcher failed with retcode $retcode" if $retcode == 1;

	pop_instrument("watcher");

	return $lines_read;
}

sub watcher_cmd_noread
{
	push_instrument("watcher");
	$ldv_watcher ||= ($ENV{'LDV_WATCHER_HOME'} || $ENV{'LDV_HOME'} || $ENV{'DSCV_HOME'})."/watcher/ldv-watcher";
	# Call watcher for the next RCV command
	my @watcher_args = ($ldv_watcher,@_);
	vsay('DEBUG',"Called watcher (output not checked): @watcher_args\n");
	system(@watcher_args);
	# Check return values
	my $rv = $?;
	my $retcode = $?>>8;
	vsay('DEBUG',"Watcher returns $retcode, waitpid: $rv\n");
	# Return code of 1 means failure.  Other codes mean useful stuff
	die "INTEGRATION ERROR: watcher failed with retcode $retcode" if $retcode == 1;

	pop_instrument("watcher");

	return (undef,$retcode);
}

1;
