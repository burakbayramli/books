#!/usr/bin/perl
# as swap1.pl but with comments inside regular expressions
# as swap3.pl but not with the regex in a string

$arg  = "[^,]+";

@cfiles = ("../py/regex/.test1.c");
foreach $cfile (@cfiles) {
    print "Treating $cfile...\n";
    open(FILE,"<$cfile") || die "Problems: $!\n";
    @lines = <FILE>;  # load all lines into a list
    close(FILE);
    $filestr = join("",@lines);
    $filestr =~ s{
        superLibFunc     # name of function to match 
           \s*           # possible whitespace
           \(            # left parenthesis
           \s*           # possible whitespace
           ($arg)        # first argument plus optional whitespace
           ,             # comma between the arguments
           \s*           # possible whitespace
           ($arg)        # second argument plus optional whitespace
           \)            # closing parenthesis
    }{superLibFunc($2, $1)}gx;
    open(FILE,">$cfile.tmp");
    print FILE $filestr;
}
