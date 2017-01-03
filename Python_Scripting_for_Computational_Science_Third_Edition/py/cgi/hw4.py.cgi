#!/store/bin/python
import sys
try:
    import cgitb; cgitb.enable()
except:
    # older Python version
    sys.stderr = sys.stdout
#print "Content-type: text/plain\n"  # useful for debugging

# old python versions will not work with this script:
if float(sys.version[0:3]) < 2.0:
    print "Content-type: text/html\n"  
    # (HTMLgen will later print the Content-type line
    # automatically, but now we need to do it manually)
    print "This script requires Python v2.0 or later"
    sys.exit(1)
# make this script find the 'HTMLgen' module (the path must
# unfortunately be hardcoded so an anybody finds our local
# python installation and its packages):
root = "/hom/inf3330/inf3330/www_docs/"
sys.path.insert(0, root + "packages/src/python/tools/HTMLgen")
from HTMLgen import *
from HTMLcolors import *

import cgi, math
form = cgi.FieldStorage()
r = form.getvalue('r', '1.2')   # 1.2 is default text
s = str(math.sin(float(r)))

# use HTMLgen to generate the Web document:
doc = SimpleDocument(title='Hello, Web World!', bgcolor=WHITE, cgi=1)
F = Form('hw4.py.cgi') # the URL of the CGI program to call

# elms is a list of form elements to be sent to InputTable for
# nice alignment (contains a table row with left label, 
# form element, and note)
elms = []  # each item is (left label, form element, note)
elms.append(('Hello, World! The sine of', 
             Input(type='TEXT', name='r', size=10, value=r), 
             ' is %g' % s))
# make this script find the 'useful' module:
sys.path.insert(0, root + 'scripting/src/tools')  
from HTMLgenutils import InputTable
F.append(InputTable(elms, # nice alignment
         leftalign='left')) 
         
F.submit = Input(type='submit', value='compute') # submit button
doc.append(F)
doc.write()
