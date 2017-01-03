#!/usr/bin/perl
$p = shift;
foreach (@ARGV) {
    next unless -f;
    open FILE, $_;
    foreach (<FILE>) { print if /$p/; }
    close FILE;
}
