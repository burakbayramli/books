#!/usr/bin/perl
die "Usage: $0 pattern file1 file2 ...\n" unless @ARGV >= 2;
($pattern, @files) = @ARGV;
foreach $_(@files) {
    if (-f $_) {
	open(FILE,"<$_");
	foreach $_ (<FILE>) {
	    if ($_ =~ /$pattern/) {
		print $_;
	    }
	}
	close(FILE);
    }
}

