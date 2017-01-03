#!/usr/bin/perl
# as swap1.pl but with comments inside regular expressions

$arg  = "[^,]+";
$call = "superLibFunc  # name of function to match 
           \\s*          # possible whitespace
           \\(           # left parenthesis
           \\s*          # possible whitespace
           ($arg)        # first argument plus optional whitespace
           ,             # comma between the arguments
           \\s*          # possible whitespace
           ($arg)        # second argument plus optional whitespace
           \\)           # closing parenthesis
           ";

@cfiles = ("../py/regex/.test1.c");
foreach $cfile (@cfiles) {
    print "Treating $cfile...\n";
    open(FILE,"<$cfile") || die "Problems: $!\n";
    @lines = <FILE>;  # load all lines into a list
    close(FILE);
    $filestr = join("",@lines);
    $filestr =~ s/$call/superLibFunc($2, $1)/gx;
    # can also write this as
    # $filestr =~ s/$call/superLibFunc(\2, \1)/gx;
    open(FILE,">$cfile.tmp");
    print FILE $filestr;

    # write out the result of a match (to demonstrate use of $1, $2, and $&):
    foreach $line (@lines) {
        if ($line =~ /$call/xg) {
            print "line: $line    match=$&  arg1=$1  arg2=$2\n";
	}
    }
}
