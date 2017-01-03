: # *-*-perl-*-*
  eval 'exec perl -w -S  $0 ${1+"$@"}' 
    if 0;  # if running under some shell

die "Usage: $0 pattern file1 file2 ...\n" if $#ARGV < 1;

# first command-line argument is the pattern to search for:
$pattern = shift @ARGV;
# run through the next command-line arguments, i.e. files, and grep:
while (@ARGV) {
    $file = shift @ARGV;
    if (-f $file) {
	open(FILE,"<$file");
	@lines = <FILE>;  # read all lines
	foreach $line (@lines) {
	    if ($line =~ /$pattern/) {
		print "$file: $line";
	    }
	}
	close(FILE);
    }
}

