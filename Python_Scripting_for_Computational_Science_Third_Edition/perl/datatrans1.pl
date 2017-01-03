: # *-*-perl-*-*
  eval 'exec perl -w -S  $0 ${1+"$@"}' 
    if 0;  # if running under some shell

die "Usage: $0 infilename outfilename\n" if $#ARGV < 1;

($infilename, $outfilename) = @ARGV;

open(INFILE,  "<$infilename");  # open for reading
open(OUTFILE, ">$outfilename"); # open for writing

# read one line at a time:
while (defined($line=<INFILE>)) {
    ($x, $y) = split(' ', $line); # extract x and y value
    $fy = myfunc($y);  # transform y value
    printf(OUTFILE "%g  %12.5e\n", $x, $fy);
}
close(INFILE); close(OUTFILE);

sub myfunc {
    my ($y) = @_;
    if ($y >= 0.0) { return $y**5.0*exp(-$y); }
    else           { return 0.0; }
}
