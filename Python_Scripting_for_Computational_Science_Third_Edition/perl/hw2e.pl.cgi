#!/home/kurs/00/inf3330/www_docs/scripting/../packages/sun5/bin/perl -w
use CGI::Debug;
use CGI;

# required opening of all CGI scripts with output:
print "Content-type: text/html\n\n";
# extract the value of the variable "r" (in the text field):
$form = CGI->new();
print "$undefined_var\n";  # warning
print "$form->param("undefined_key")\n";  
# opening a file will normally be illegal for a CGI script
# (the script has write permissions as "others"):
open(FILE, ">myfile") or die "Cannot open myfile!";

if (defined($form->param("r"))) {
    $r = $form->param("r"); $s = sin($r);
} else {
    $r = "1.2"; $s = "";  # default
}
# print form with value:
print <<EOF;
<HTML><BODY BGCOLOR="white">
<FORM ACTION="hw2e.pl.cgi" METHOD="POST">
Hello, World! The sine of 
<INPUT TYPE="text" NAME="r" SIZE="10" VALUE="$r">
<INPUT TYPE="submit" VALUE="equals" NAME="equalsbutton"> $s
</FORM></BODY></HTML>
EOF
