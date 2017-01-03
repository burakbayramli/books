#!/usr/bin/perl
$arg = "[^,]+";
$call = "superLibFunc\\s*\\(\\s*($arg)\\s*,\\s*($arg)\\s*\\)";
print $call;

@cfiles = ("../py/regex/.test1.c");
foreach $cfile (@cfiles) {
    print "Treating $cfile...\n";
    open(FILE,"<$cfile") || die "cannot open file: $!\n";
    @lines = <FILE>;  # load all lines into a list
    close(FILE);
    $filestr = join("",@lines);
    $filestr =~ s/$call/superLibFunc($2, $1)/g;
    open(FILE,">$cfile.tmp");
    print FILE $filestr;
}
