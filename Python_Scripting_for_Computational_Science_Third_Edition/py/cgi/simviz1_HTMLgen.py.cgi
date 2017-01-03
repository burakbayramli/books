#!/hom/inf3330/www_docs/packages/SunOS/bin/python
# Note: this python applies dynamic libraries, which demands
# LD_LIBRARY_PATH to be correctly set (run the script in
# a shell wrapper where correct envir. vars are set)
import sys, cgi, math, os

sys.stderr = sys.stdout
# debugging problem: doc.write at the end writes the
# Content-type; we need it before for debugging
# and getting error messages on the screen
# print 'Content-type: text/html\n'

from HTMLgen import *
from HTMLcolors import *
from HTMLgenutils import InputTable
    
# make this script find the 'useful' module:
root = '/hom/inf3330/www_docs/'
sys.path.insert(0,root+'scripting/src/tools')  

# all the input variables and their default values:
m = 1.0; b = 0.7; c = 5.0; func = 'y'; A = 5.0; w = 2*math.pi
y0 = 0.2; tstop = 30.0; dt = 0.05; case = 'tmp1'

form = cgi.FieldStorage()
if form.has_key('m'):      m    = float(form['m'].value)
if form.has_key('b'):      b    = float(form['b'].value)
if form.has_key('c'):      c    = float(form['c'].value)
if form.has_key('func'):   func = form['func'].value
if form.has_key('A'):      A    = float(form['A'].value)
if form.has_key('w'):      w    = flost(form['w'].value)
if form.has_key('y0'):     y0   = float(form['y0'].value)
if form.has_key('tstop'):  tstop= float(form['tstop'].value)
if form.has_key('dt'):     dt   = float(form['dt'].value)
case = 'tmp1'  # not necessary to be changed by the user

# use HTMLgen to generate the Web document:
doc = SimpleDocument(title='Oscillator', bgcolor=WHITE, cgi=1)

# place a sketch of the problem being solved, placed to the
# left of the form:
doc.append(Image(os.path.join(os.pardir,os.pardir,'gui','figs',
                              'simviz.xfig.gif'), align='left'))

# the URL of the CGI program to call
F = Form(cgi='wrapper.sh.cgi?s=simviz1_HTMLgen.py.cgi') 

# elms is a list of form elements to be sent to InputTable for
# nice alignment (contains a table row with left label, 
# form element, and note)
elms = []  # each item is (left label, form element, note)
elms.append(('m',    Input(name='m',    size=10, value=m), ''))
elms.append(('b',    Input(name='b',    size=10, value=b), ''))
elms.append(('c',    Input(name='c',    size=10, value=c), '')) 
elms.append(('func', Input(name='func', size=10, value=func), ''))
elms.append(('A',    Input(name='A',    size=10, value=A), ''))
#elms.append(('w',    Input(name='w',    size=10, value=w), ''))
# on our server we got a strange error that disappeared when
# the name of the w field was changed to ww:
# (sometimes the original name w worked well...)
elms.append(('w',    Input(name='ww',   size=10, value=w), ''))
elms.append(('y0',   Input(name='y0',   size=10, value=y0), '')) 
elms.append(('tstop',Input(name='tstop',size=10, value=tstop), ''))
elms.append(('dt',   Input(name='dt',   size=10, value=dt), ''))

F.append(InputTable(elms, # nice alignment
         leftcolor=WHITE, centercolor=WHITE, notecolor=WHITE,
         leftalign='left')) 

# submit button:
F.submit = Input(type='submit', value='simulate and visualize') 
doc.append(F)

eq = """
%(m)gDDy + %(b)gDy + %(c)g%(func)s = %(A)gcos(%(w)g*t),
y(0)=%(y0)g, Dy(0)=0
""" % vars()
doc.append(HR())  # horizontal rule (<HR>)
doc.append(Paragraph())
doc.append(Blockquote(eq))  # equation in a BLOCKQUOTE

# command for running the simulation program:
simviz_script = os.path.join(os.pardir,os.pardir,'intro',
                             'python','simviz1.py')
cmd = simviz_script + \
      ' -m %g -b %g -c %g -func %s -A %g -w %g'\
      ' -y0 %g -tstop %g -dt %g -case %s' % \
      (m,b,c,func,A,w,y0,tstop,dt,case)


# if the form has received data, we run the simulator and create plot:

if form.has_key('m'):
    # run simulator and create plot:

    # important details:
    #   correct path to simviz1.py (executable for all)
    #   the current directory must have write permissions
    #       for anyone (chmod a+rwx .)
    #   make sure that anyone can delete the case subdir. 

    os.system(cmd)
    os.chmod(case, 0777)  # anyone has write permission
        
    # show PNG image:
    imgfile = os.path.join(case,case+'.png')
    if os.path.isfile(imgfile):
        doc.append(Image(imgfile, align='top'))

doc.write()


