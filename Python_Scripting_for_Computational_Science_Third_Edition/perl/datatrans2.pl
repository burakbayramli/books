: # *-*-perl-*-*
  eval 'exec perl -w -S  $0 ${1+"$@"}' 
    if 0;  # if running under some shell

die "Usage: $0 infilename outfilename\n" if $#ARGV < 1;
($infilename, $outfilename) = @ARGV;

open(INFILE,  "<$infilename") 
    or die "unsuccessful opening of $infilename; $!\n";
@lines = <INFILE>; # read file into array of lines
close(INFILE);

# go through each line and split line into x and y columns:
@x = (); @y = ();   # store data pairs in two arrays x and y
for $line (@lines) {
    ($xval, $yval) = split(' ', $line);
    push(@x, $xval);  push(@y, $yval);
}

open(OUTFILE, ">$outfilename")
    or die "unsuccessful opening of $outfilename; $!\n";

for ($i = 0; $i <= $#x; $i++) {
    $fy = myfunc($y[$i]);  # transform y value
    printf(OUTFILE "%g  %12.5e\n", $x[$i], $fy);
}
close(OUTFILE);

sub myfunc {
    my ($y) = @_;
    if ($y >= 0.0) { return $y**5.0*exp(-$y); }
    else           { return 0.0; }
}
