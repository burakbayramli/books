#!/store/bin/python
import cgi, math, sys
import cgitb; cgitb.enable()
    
# required opening of all CGI scripts with output:
print 'Content-type: text/html\n'

form = cgi.FieldStorage()

print form.getvalue('undefined_key')  # error
# no CGI script is (normally) allowed to open a new file:
file = open('myfile', 'w')  # error

form = cgi.FieldStorage()
if form:                         # is the form is filled out?
    r = form.getvalue('r')
    s = str(math.sin(float(r)))
else:
    s = ''
    r = ''

# print form:
print """
<HTML><BODY BGCOLOR="white">
<FORM ACTION="hw2e.py.cgi" METHOD="POST">
Hello, World! The sine of 
<INPUT TYPE="text" NAME="r" SIZE="10" VALUE="%s">
<INPUT TYPE="submit" VALUE="equals" NAME="equalsbutton"> %s
</FORM></BODY></HTML>
""" % (r,s)
