#!/usr/bin/perl
die "Usage: $0 pattern file1 file2 ...\n" unless @ARGV >= 2;
($pattern, @files) = @ARGV;
foreach $file (@files) {
    if (-f $file) {
	open FILE, "<$file";
	@lines = <FILE>;
	@match = grep /$pattern/, @lines;  # Perl grep
	print "$file: @match";
	close FILE;
    }
}

