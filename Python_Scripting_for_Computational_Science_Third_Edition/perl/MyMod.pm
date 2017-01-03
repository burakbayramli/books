# module file: MyMod.pm

package MyMod;

$logfile = "";  # variable shared among subroutines

sub set_logfile {
    ($logfile) = @_;
    print "logfile=$logfile\n";
}

sub another_routine {
    print "inside another_routine\n";
    for $arg (@_) { print "argument=$arg\n"; }
}

1;
