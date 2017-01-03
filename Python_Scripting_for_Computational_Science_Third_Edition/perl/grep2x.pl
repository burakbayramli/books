#!/usr/bin/perl
$pattern = shift;  # shift; means shift @ARGV
while (<>) {       # read line by line in file by file
    print if /$pattern/o;  # o increases the efficiency
}

