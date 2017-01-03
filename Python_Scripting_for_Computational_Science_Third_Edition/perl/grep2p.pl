#!/usr/bin/perl
die "Usage: $0 pattern file1 file2 ...\n" unless @ARGV >= 2;
($pattern, @files) = @ARGV;
foreach (@files) {
    next unless -f;
    open FILE, $_;
    foreach (<FILE>) { print if /$pattern/; }
    close FILE;
}
