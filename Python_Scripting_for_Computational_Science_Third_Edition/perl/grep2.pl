#!/usr/bin/perl
die "Usage: $0 pattern file1 file2 ...\n" unless @ARGV >= 2;
($pattern, @files) = @ARGV;
foreach (@files) {
    if (-f) {
	open(FILE,"<$_");
	foreach (<FILE>) {
	    if (/$pattern/) {
		print;
	    }
	}
	close(FILE);
    }
}

