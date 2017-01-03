#!/usr/bin/awk -f

#
# This awk script randomly generates simple log file entries.  It expects
# a single line of input with the following tokens:
#  IP[:IP...] DATE[:DATE/...] STATUS[:STATUS...] NUM_LINES
#
#  127.0.0.1 - - [23/Jun/2009:10:39:11 +0300] "GET / HTTP/1.1" 500 606
#
#
# Example usage:  Generate 200 log entries randomly combining the
#                 IP's, dates and HTTP status codes
#
# echo "127.0.0.1:10.192.12.2:10.194.3.21:192.124.1.2 10/Apr/2007:11/Apr/2007 200:202:404:500 200" | ./gen-large-logs.awk

{
    split($1,ips,":");
    split($2,dates,":");
    split($3,statuses,":");

    numlines=$4;

    for(i=0; i < numlines; i++) {
        ip = ips[1 + int(rand() * 1000) % length(ips)];
        date = dates[1 + int(rand() * 1000) % length(dates)];
        status = statuses[1 + int(rand() * 1000) % length(statuses)];
        print ip" - - ["date"] \"GET "resource" HTTP/1.1\" "status" "bytes
    }

}
