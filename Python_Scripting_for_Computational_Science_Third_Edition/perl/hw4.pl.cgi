#!/usr/local/bin/perl
use CGI qw/:standard/;
print header,
    start_html(-title=>"Hello, Web World!",
	       -BGCOLOR=>'white'),
    start_form,
    "Hello, World! The sine of ",
    textfield(-name=>'r', -default=>1.2, -size=>10), 
    "\n", submit(-name=>'equals'), " ";
if (param()) { $r = param("r"); $s = sin($r); }
else { $s = sin(1.2); }
print $s, "\n", end_form, end_html, "\n";
