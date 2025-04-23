#!/usr/bin/perl -w

# Perl script for testing a CSCE 355 project submission on a linux lab box

# Usage:
# $ project-test.pl [your-submission-root-directory]
#
# The directory argument is optional.  If not there, then the default is
# $default_submission_root, defined below.

# PLEASE NOTE: This script normally (over)writes the file "comments.txt"
# in your submission root directory.  If that already exists, the existing
# file is first changed to "comments.bak".  The script also (over)writes
# "errlog.txt" in your submission root directory.

# This script must be run under the bash shell.


######## Edit the following to reflect your directory structure (mandatory):

# directory containing all the test files
# (edit this for self-testing; when we grade, it will point to our version)
$test_files_root = "$ENV{HOME}/public_html/csce355/prog-proj2/sp25/test";

# Directory that contains the utilities in2post and pre2in
# (This should be an absolute pathname, so that you don't need
# to make $utils a subdirectory of the submission root or include it
# in your $PATH variable).
$utils = "$ENV{HOME}/bin";

########### Editing below this line is optional but recommended. #############

# Edit to point to your submission root directory, i.e., the directory
# containing your "build-run.txt" file, if you don't want to specify it
# on the command line every time.
$submission_root = "$ENV{HOME}/public_html/csce355/prog-proj2/sp25/solution";

# List of possible command line arguments (without the initial "--").
# Comment out any of these you are not ready to test.
@command_line_args_array = (
    'no-op',
    'simplify',
    'empty',
    'has-epsilon',
    'has-nonepsilon',
    'uses',
    'not-using',
    'infinite',
    'starts-with',
    'reverse',
    'ends-with',
    'prefixes',
    'bs-for-a',
    'insert',
    'strip',
);

# Hash of the possible command line arguments and their types
%command_line_args_hash = (
    'no-op'          => 'transform',
    'simplify'       => 'transform',
    'empty'          => 'boolean',
    'has-epsilon'    => 'boolean',
    'has-nonepsilon' => 'boolean',
    'uses'           => 'boolean symbol',
    'not-using'      => 'transform',
    'infinite'       => 'boolean',
    'starts-with'    => 'boolean symbol',
    'reverse'        => 'transform',
    'ends-with'      => 'boolean symbol',
    'prefixes'       => 'transform',
    'bs-for-a'       => 'transform',
    'insert'         => 'transform symbol',
    'strip'          => 'transform symbol',
);

# NOTE: There is only one test file: input.txt, or its postfix equivalent,
# input-postfix.txt.
# If you want to add new tests, append them to input.txt, then rerun the
# command
#   $ in2post < input.txt > input-postfix.txt

# This is the subdirectory of $submission_root
# where the script temporarily stores the results of running your code.
# Change this if you want them placed somewhere else.  You should NOT
# set this to be the same as your test directory; otherwise it will clobber
# the files used to compare with your outputs!
$test_outputs = "test-outputs";

# Flag to control deletion of temporary files, i.e., the output files
# created by running your program
# A nonzero value means the $test_outputs directory is deleted after it is used;
# a zero value means nothing will be deleted (but they will be
# overwritten on subsequent executions of this script).
# This flag has NO effect on any files created by running your program if
# it times out (that is, exceeds the $timeout limit, below); those files will
# ALWAYS be deleted.
# Set this value to 0 if you want to examine your own program's outputs as
# produced by this script.

# Set to 0 by default.  -SF 4/4/2025 @ 3:00pm
$delete_temps = 0;

# Time limit for each run of your program (in seconds).  This is the value
# I will use when grading, but if you want to allow more time in the testing
# phase, increase this value.
$timeout = 11;     # seconds


############## You should not need to edit below this line. ###############

# Keys are tasks attempted;
# values are the corresponding point values.
%progress = ();

# Holds build and run commands for the program
%build_run = ();

# Check existence and readability of the test files directory
die "Test files directory $test_files_root\n  is inaccessible\n"
    unless -d $test_files_root && -r $test_files_root;

#sub main
{
    if (@ARGV) {
	$udir = shift @ARGV;  # The directory argument on the command line
	$udir =~ s/\/$//;     # strip the trailing "/" if it is there
	$udir ne "" or die "Cannot use root directory\n";
    }
    else {
	$udir = $submission_root;
    }
    # $udir should now be the home directory for your submission.
    
    $uname = "self-test";
    process_user();
}


sub process_user {
    print "Processing user $uname\n";

    die "No accessible directory $udir ($!)\n"
	unless -d $udir && -r $udir && -w $udir && -x $udir;

    die "Cannot change to directory $udir ($!)\n"
	unless chdir $udir;

    print "Current working directory is $udir\n";

    # Copy STDOUT and STDERR to errlog.txt in $udir
    open STDOUT, "| tee errlog.txt" or die "Can't redirect stdout\n";
    open STDERR, ">&STDOUT" or die "Can't dup stdout\n";
    select STDERR; $| = 1;	# make unbuffered
    select STDOUT; $| = 1;	# make unbuffered

    if (-e "comments.txt") {
	print "comments.txt exists -- making backup comments.bak\n";
	rename "comments.txt", "comments.bak";
    }

    # Set up the comments stream
    open(COMMENTS, "> comments.txt");

    cmt("Comments for $uname -------- " . now() . "\n");

    # Create the directory for test outputs if it doesn't already exist
    mkdir $test_outputs
	unless -d $test_outputs;

    cmt("parsing the build-run.txt file ...");
    if (parse_build_run()) {
	cmt("ERROR PARSING build-run.txt ... quitting\n");
	exit(1);
    }
    cmt(" done\n\n");
    cmt("building executable ...\n");
    $rc = 0;
    foreach $command (@{$build_run{BUILD}}) {
	if ($command =~ /\s*make(\s|$)/ && $command !~ /-B/) {
	    cmt("    Changing command \"$command\" to ");
	    $command =~ s/\s*make/make -B/;
	    cmt(" \"$command\"\n");
	}
	else {
	    cmt("  $command\n");
	}
	$rc = system($command);
	if ($rc >> 8) {
	    cmt("    FAILED ... quitting\n");
	    exit(1);
	}
	cmt("    succeeded\n");
    }
    $base_command = $build_run{RUN};
    cmt(" done\n    base command is \"$base_command\"\n\n");

    $in_file = "$test_files_root/input-postfix.txt";

    # Create the postfix input file, whether or not it already exists
    $in2post_command = "$utils/in2post < $test_files_root/input.txt > $in_file";
    cmt("Creating the file input-postfix.txt using the command\n");
    cmt("    $in2post_command\n");
    $rc = system($in2post_command);
    if ($rc >> 8) {
	cmt(" FAILED (quitting)\n");
	exit(1);
    }
    cmt("    done\n\n");

    foreach $arg (@command_line_args_array) {
	test_task($arg);
    }

    report_summary();

    rmdir $test_outputs if $delete_temps;

    close COMMENTS;

    print "\nDone.\nComments are in $udir/comments.txt\n\n";
}


sub test_task {
    my ($arg) = @_;
    my $out_base;
    my $symb_command;

    cmt("testing arg --$arg ...\n");

    my @tasks = ();
    my $command;
    $command = "$base_command --$arg";
    
    # If there is no symbol as an extra command line argument ...
    if ($command_line_args_hash{$arg} !~ /symbol/) {
	if ($command_line_args_hash{$arg} =~ /^transform/) {
	    $command = "(" . $command;
	    $command .= " | $utils/pre2in)";
	}
	$command .= " < $in_file > $test_outputs/$arg.txt";
	$command .= " 2> $test_outputs/$arg-err.txt";
	push @tasks, ($command);
    }
    else {    # the program takes a symbol as an extra command line argument
	foreach $symb ('a','b','c','d','e','f') {
	    $symb_command = $command . " $symb";
	    # If the intended outputs are regexes ...
	    if ($command_line_args_hash{$arg} =~ /^transform/) {
		$symb_command = "(" . $symb_command;
		$symb_command .= " | $utils/pre2in)";
	    }
	    $symb_command .= " < $in_file > $test_outputs/$arg-$symb.txt";
	    $symb_command .= " 2> $test_outputs/$arg-$symb-err.txt";
	    push @tasks, ($symb_command);
	}
    }

    $progress{$arg} = 1;
    foreach $task (@tasks) {

	$task =~ /\/([^\/.]+)\.txt 2>/; $out_base = $1;
	cmt("  Running the command:\n  $task\nOutput base name: $out_base\n");
	eval {
	    local $SIG{ALRM} = sub { die "TIMED OUT\n" };
	    alarm $timeout;
	    $rc = system("$task");
	    alarm 0;
	};
	if ($@ && $@ eq "TIMED OUT\n") {
	    cmt("    $@");		# program timed out before finishing
	    unlink "$test_outputs/$out_base.txt"
		if -e "$test_outputs/$out_base.txt";
	    unlink "$test_outputs/$out_base-err.txt"
		if -e "$test_outputs/$out_base-err.txt";
	    $progress{$arg} = 0;
	    next;
	}
	if ($rc >> 8) {
	    cmt("    (terminated with nonzero status (status ignored))\n");
	}
	else {
	    cmt("    (terminated with zero status (status ignored))\n");
	}
        error_report("$test_outputs/$out_base");

	if (!(-e "$test_outputs/$out_base.txt")) {
	    cmt("  OUTPUT FILE $test_outputs/$out_base.txt DOES NOT EXIST\n");
	    $progress{$arg} = 0;
	    next;
	}

	cmt("  $test_outputs/$out_base.txt exists---comparing with solution\n");

	$report = check_outcomes($out_base);
	unlink "$test_outputs/$out_base.txt" if $delete_temps;
	chomp $report;
	if ($report eq '') {
	    cmt("  outcomes match (correct)\n");
	}
	else {
	    cmt("  OUTCOMES DIFFER:\nvvvvv\n$report\n^^^^^\n");
	    $progress{$arg} = 0;
	}
    }

    rmdir $test_outputs if $delete_temps;
    cmt("done\n\n");
}


# Sets build_run hash to the building and execution commands for this program
# Returns nonzero if error
sub parse_build_run {
    $br_file = "build-run.txt";
    open BR, "< $br_file"
	or die "Cannot open $br_file for reading ($!)\n";
    get_line(1) or return 1;
    $line = eat_comments();
    if ($line !~ /^\s*Build:\s*$/i) {
	cmt("NO Build SECTION FOUND; ABORTING PARSE\n");
	return 1;
    }
    get_line(1) or return 1;
    $line = eat_comments();
    $build_run{BUILD} = [];
    while ($line ne "" && $line !~ /^\s*Run:\s*$/i) {
	$line =~ s/^\s*//;
	push @{$build_run{BUILD}}, $line;
	get_line(1) or return 1;
	$line = eat_comments();
    }
    if ($line eq "") {
	cmt("NO Run SECTION FOUND; ABORTING PARSE\n");
	return 1;
    }
    # This is now true: $line =~ /^\s*Run:\s*$/i
    get_line(1) or return 1;
    $line = eat_comments();
    $line =~ s/^\s*//;
    $build_run{RUN} = $line;
    get_line(0) or return 0;
    $line = eat_comments();
    if ($line ne "") {
	cmt("EXTRA TEXT IN FILE; ABORTING PARSE\n");
	return 1;
    }
    close BR;
    return 0;
}


sub get_line {
    my ($flag) = @_;
    return 1
	if defined($line = <BR>);
    if ($flag) {
	cmt(" FILE ENDED PREMATURELY\n");
    }
    return 0;
}


# Swallow comments and blank lines
sub eat_comments {
    chomp $line;
    while ($line =~ /^\s*#/ || $line =~ /^\s*$/) {
	$line = <BR>;
	defined($line) or return "";
	chomp $line;
    }
    return $line
}


sub check_outcomes {
    my ($out_base) = @_;
    my $diff;

    cmt("Running \"diff\" on your output and the solution:\n");
    cmt("  diff $test_outputs/$out_base.txt $test_files_root/$out_base.txt\n");
    $diff = `diff $test_outputs/$out_base.txt $test_files_root/$out_base.txt`;
    return $diff;
}


sub error_report {
    my ($base) = @_;
    if (-e "${base}-err.txt") {
	if (-s "${base}-err.txt") {
	    cmt("  standard error output:\nvvvvv\n");
	    $report = `cat ${base}-err.txt`;
	    chomp $report;
	    cmt("$report\n^^^^^\n");
	}
	unlink "${base}-err.txt" if $delete_temps;
    }
}


sub report_summary {
    my $report;
    my $arg;
    my $point_value;
    my $sum = 0;
    cmt("######################################################\n");
    cmt("Summary for $uname:\n\n");

    foreach $arg (@command_line_args_array) {
	$point_value = ($arg eq 'no-op' ? 35 : $arg eq 'simplify' ? 10 : 5)
	    * $progress{$arg};
	cmt("$arg:     $point_value points\n");
	$sum += $point_value;
    }
    # Truncate any extra credit
    $sum = 100 if $sum > 100;
    cmt("TOTAL POINTS: $sum/100\n");
    cmt("######################################################\n");
}


sub cmt {
    my ($str) = @_;
#  print $str;
    print(COMMENTS $str);
}


sub now {
    my $ret;

    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime;
    $ret = ('Sun','Mon','Tue','Wed','Thu','Fri','Sat')[$wday];
    $ret .= " ";
    $ret .= ('Jan','Feb','Mar','Apr','May','Jun','Jul',
	     'Aug','Sep','Oct','Nov','Dec')[$mon];
    $ret .= " $mday, ";
    $ret .= $year + 1900;
    $ret .= " at ${hour}:${min}:${sec} ";
    if ( $isdst ) {
	$ret .= "EDT";
    } else {
	$ret .= "EST";
    }
    return $ret;
}
