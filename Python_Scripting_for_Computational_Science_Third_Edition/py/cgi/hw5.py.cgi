#!/hom/inf3330/www_docs/packages/SunOS/bin/python
# Note: this python applies dynamic libraries, which demands
# LD_LIBRARY_PATH to be correctly set (run the script in
# a shell wrapper where correct envir. vars are set)

# typical URL used to call the present script:
# http://www.some.where/some/dir/wrapper.sh.cgi?s=hw5.py.cgi

import sys
try:
    import cgitb; cgitb.enable()
except:
    # older Python version
    sys.stderr = sys.stdout
#print 'Content-type: text/plain\n'  # useful for debugging

# this script should find the below modules since we expect
# that PYTHONPATH is set correctly in the shell wrapper

from HTMLgen import *
from HTMLcolors import *
from HTMLgenutils import InputTable

import cgi, math
form = cgi.FieldStorage()
r = form.getvalue('r', '1.2')   # 1.2 is default text
s = str(math.sin(float(r)))

# use HTMLgen to generate the Web document:
doc = SimpleDocument(title='Hello, Web World!', bgcolor=WHITE, cgi=1)
# do not forget to include the wrapper here:
F = Form('wrapper.sh.cgi?s=hw5.py.cgi')

# elms is a list of form elements to be sent to InputTable for
# nice alignment (contains a table row with left label, 
# form element, and note)
elms = []  # each item is (left label, form element, note)
elms.append(('Hello, World! The sine of', 
             Input(type='TEXT', name='r', size=10, value=r), 
             ' is %g' % s))
F.append(InputTable(elms, # nice alignment
         leftalign='left')) 
         
F.submit = Input(type='submit', value='compute') # submit button
doc.append(F)
doc.write()
