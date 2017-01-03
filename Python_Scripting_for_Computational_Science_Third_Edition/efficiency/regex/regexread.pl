# find no of lines in the file (= no of items in the array)
open F, "<$ARGV[0]";
$nlines = 0;
while (<F>) { $nlines += 1; }
close F;

$n = int($nlines**(1/3.0) + 0.5);  # careful rounding!
$total_length = $nlines;
# use 1D array here
@a = (0.0)x$total_length;
print "$n\n";

open F, "<$ARGV[0]";
while (<F>) {
    if (/\[(\d+),(\d+),(\d+)\]=(.*)/) {
	$i = $1*$n*$n + $2*$n + $3;
	$a[$i] = $4;
	#print "[$1,$2,$3] -> $i = $4\n";
    }
}
close F;
print "a[3]=$a[3]\n";
