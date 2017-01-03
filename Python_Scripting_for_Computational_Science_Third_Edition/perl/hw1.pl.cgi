#!/usr/local/bin/perl
use CGI;
# required opening of all CGI scripts with output:
print "Content-type: text/html\n\n";
# extract the value of the variable "r" (in the text field):
$form = CGI->new();
$r = $form->param("r");  $s = sin($r);
# print answer (very primitive HTML code):
print "Hello, World! The sine of $r equals $s\n";
