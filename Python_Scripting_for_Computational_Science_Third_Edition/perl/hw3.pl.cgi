#!/usr/local/bin/perl
use CGI;
$wp = CGI->new();
print $wp->header,
    $wp->start_html(-title=>"Hello, Web World!",
		    -BGCOLOR=>'white'),
    #$wp->start_form(-action=>'hw3.pl.cgi'), # default
    $wp->start_form,
    "Hello, World! The sine of ",
    $wp->textfield(-name=>'r', -default=>1.2, -size=>10), 
    "\n", $wp->submit(-name=>'equals'), " ";
if ($wp->param()) {
    $r = $wp->param("r"); $s = sin($r);
} else { $s = sin(1.2); }
print $s, "\n", $wp->end_form,
    $wp->end_html, "\n";
