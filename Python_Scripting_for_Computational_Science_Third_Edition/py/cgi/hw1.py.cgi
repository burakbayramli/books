#!/store/bin/python
import cgi, math

# required opening of CGI scripts with HTML output:
print 'Content-type: text/html\n'

# extract the value of the variable "r" (in the text field):
form = cgi.FieldStorage()
r = form.getvalue('r');  

s = str(math.sin(float(r)))
print 'Hello, World! The sine of %s equals %s' % (r,s)
