#!/usr/bin/env perl
# Perl modules:
@perlmodules = ('Tk', 'CGI::QuickForm', 'LWP::Simple',
		'Tar', 'Algorithm::Diff');
for $module (@perlmodules) {
    system "perl -e 'use $module' $out2trash";
    if ($?) {
	print "*** Perl module $module is not available...\n";
    } else {
	print "You have the Perl module $module...\n";
    }
}
# perl -e 'use CGI::Debug' exits with 1:
#system "perl -e 'use CGI::Debug' > /dev/null";
#if ($? >= 2) {
#    print "*** Perl module CGI::Debug is not available...\n";
#} else {
#    print "You have the Perl module CGI::Debug...\n";
#}

print "\n";
