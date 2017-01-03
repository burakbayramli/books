#!/store/bin/python
import sys
try:
    import cgitb; cgitb.enable()
except:
    # older Python version
    sys.stderr = sys.stdout
#print 'Content-type: text/plain\n'  # useful for debugging

# make this script find the 'HTMLgen' module (the path must
# unfortunately be hardcoded so a nobody/www finds our local
# python installation and its packages):
root = '/hom/inf3330/www_docs/'
sys.path.insert(0, root + 'packages/src/python/tools/HTMLgen')
from HTMLgen import *
from HTMLcolors import *

import cgi, math
form = cgi.FieldStorage()
r = form.getvalue('r', '1.2')
s = str(math.sin(float(r)))

# use HTMLgen to generate the Web document:
doc = SimpleDocument(title='Hello, Web World!', bgcolor=WHITE, cgi=1)
F = Form('hw3.py.cgi') # the URL of the CGI program to call
F.append(Input(type='TEXT', # one-line text
               name='r', size=10, value=r, 
               # label to the left of the text field:
               llabel='Hello, World! The sine of ',
               # label to the right of the text field:
               rlabel=' is %g' % s))
F.submit = Input(type='submit', value='compute') # submit button
doc.append(F)
doc.write()

"""
Note that the layout of this Web page is a bit different from that
made by hw2.py.cgi.

The doc/index.html page provides a link for downloading the HTMLgen
module and its documentation. The documentation (start with main.html,
not index.html) contains a brief introduction to HTMLgen and a full
reference manual. For the novice, the source code in the HTMLtest.py
script, which actually tests all the examples in the HTMLgen
documentation, contains lots of good illustrations on the usage of
HTMLgen.  Search for Form in the HTMLtest.py file to see how the
different form elements in the ``Forms'' page of the HTMLgen
documentation are generated. Together with the HTMLgen reference
manual this will be enough information to work further with
construction of forms using HTMLgen.


Forms with many elements normally employ a table to organize the
layout.  HTMLgen does not support this directly, but the small Python
classes InputTable, CheckBoxes, and RadioButtons, taken from the Quick
Python book (Ch. 25), can be used to organize form elements in a table
fashion. These classes have been included in the HTMLgenutils module
in src/tools and provide some of the layout functionality in Perl's
CGI::QuickForm module.  A trivial example on the usage of class
InputTable is shown in hw4.py.cgi.

For documentation of InputTable and other formatting classes,
we refer to the Quick Python book or the source code in
src/tools/py4scutils.py.
To test the CGI script on a Unix server, run

./hw4.py.cgi > tmp.html

and load the tmp.html file in a browser. You can then view the
layout of the form elements (as usual in such testing, the opening
Content-Type: text/html output is just to be ignored as it will
disappear when the script is run within a Web browser).

hw5.py.cgi is the same script as hw4.py.cgi except that we run
it under a local Python interpreter which loads shared libraries.
This requires us to run hw5.py.cgi under a shell wrapper.
"""
