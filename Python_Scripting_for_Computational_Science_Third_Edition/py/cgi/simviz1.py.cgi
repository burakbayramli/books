#!/store/bin/python
import sys, cgi, math, os
"""
Web interface to the simviz1.py script.
Necessary PATH values are set in the script, i.e.,
there is no need to run the script through a wrapper.
"""
try:
    import cgitb; cgitb.enable()
except:
    # older Python version
    sys.stderr = sys.stdout
print 'Content-type: text/html\n'

class FormParameters:
    """Easy handling of a set of form parameters."""

    def __init__(self, form):
        self.form = form     # a cgi.FieldStorage() object
        self.parameter = {}  # contains all parameters

    def set(self, name, default_value=None):
        """Register a new parameter."""
        self.parameter[name] = default_value
 
    def get(self, name):
        """Return the value of the form parameter name."""
        if name in self.form:
            self.parameter[name] = self.form.getvalue(name)

        if name in self.parameter:
            return self.parameter[name]
        else:
            return "No variable with name '%s'" % name

    def tablerow(self, name):
        """Print a form entry in a table row."""
        print """
        <TR>
        <TD>%s</TD>
        <TD><INPUT TYPE="text" NAME="%s" SIZE=10 VALUE="%s">
        </TR>
        """ % (name, name, self.get(name))

    def tablerows(self):
        """Print all parameters in a table of form text entries."""
        print '<TABLE>'
        for name in self.parameter.keys():
            self.tablerow(name)
        print '</TABLE>'

form = cgi.FieldStorage()
p = FormParameters(form)
p.set('m', 1.0)  # set 'm' with default value 1.0
p.set('b', 0.7)
p.set('c', 5.0)
p.set('func', 'y')
p.set('A', 5.0)
p.set('w', 2*math.pi)
p.set('y0', 0.2)
p.set('tstop', 30.0)
p.set('dt', 0.05)
case = 'tmp_%d' % os.getpid()

# start writing HTML:
print """
<HTML><BODY BGCOLOR="white">
<TITLE>Oscillator code interface</TITLE>
<IMG SRC="%s" ALIGN="left">
<FORM ACTION="simviz1.py.cgi" METHOD="POST">
""" % \
(os.path.join(os.pardir,os.pardir,'misc','figs','simviz.xfig.gif'))
# define all form fields:
p.tablerows()
print """
<INPUT TYPE="submit" VALUE="simulate and visualize" NAME="sim">
</FORM>
"""

# make sure that simviz1.py finds the oscillator code and the
# gnuplot program, i.e., define absolute path to these codes
# and add to PATH:
root = '/hom/inf3330/www_docs/'
osc = root + 'scripting/Linux/bin'
gnuplot = root + 'packages/Linux/bin'
other = '/local/bin:/usr/bin:/bin'
os.environ['PATH'] = os.pathsep.join\
    ([os.environ['PATH'], osc, gnuplot, other])

if not os.path.isfile(osc+'/oscillator'):
    print 'The oscillator program was not found '\
          'so it is impossible to perform simulations'
if not os.access(os.curdir, os.W_OK):
    print 'Current directory has not write permissions '\
          'so it is impossible to perform simulations'

# command-line arguments for the simulation program:
#cmd = '/store/bin/python ' + simviz_script + \
cmd = \
      ' -m %s -b %s -c %s -func %s -A %s -w %s'\
      ' -y0 %s -tstop %s -dt %s -case %s -noscreenplot' % \
      (p.get('m'), p.get('b'), p.get('c'),
       p.get('func'), p.get('A'), p.get('w'),
       p.get('y0'), p.get('tstop'), p.get('dt'), case)

if form:             # run simulator and create plot
    script = os.path.join(os.pardir, 'intro', 'simviz1.py')
    oscmd = 'python ' + script + ' ' + cmd
    import commands  # safest to grab all output
    failure, outtext = commands.getstatusoutput(oscmd)
    if failure:
        print 'Could not run simviz1.py with<br>', oscmd
    else:
        print 'Successful run of simulation'
    # see simviz1w.py.cgi for alternative with import simviz1.py
    os.chmod(case, 0777) # make sure anyone can delete/write subdir
        
    # show PNG image:
    imgfile = os.path.join(case,case+'.png')
    if os.path.isfile(imgfile):
        # make an arbitrary new filename to prevent that browsers
        # may reload the image from a previous run:
        import random
        newimgfile = os.path.join(case,
                     'tmp_'+str(random.uniform(0,2000))+'.png')
        os.rename(imgfile, newimgfile)
        print """<IMG SRC="%s">""" % newimgfile
print '</BODY></HTML>'
# end
