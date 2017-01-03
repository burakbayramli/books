#!/usr/local/bin/perl
use CGI;
# required opening of all CGI scripts with output:
print "Content-type: text/html\n\n";
# extract the value of the variable "r" (in the text field):
$form = CGI->new();
if (defined($form->param("r"))) {
    $r = $form->param("r"); $s = sin($r);
} else {
    $r = "1.2"; $s = "";  # default
}
# print form with value:
print <<EOF;
<HTML><BODY BGCOLOR="white">
<FORM ACTION="hw2.pl.cgi" METHOD="POST">
Hello, World! The sine of 
<INPUT TYPE="text" NAME="r" SIZE="10" VALUE="$r">
<INPUT TYPE="submit" VALUE="equals" NAME="equalsbutton"> $s
</FORM></BODY></HTML>
EOF

