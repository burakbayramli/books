#!/local/snacks/bin/python
"""
As simviz1pyCGI.py.cgi, but parameters have dimensions
(just inherit SimViz from simviz1cp_unit instead of from
simviz1cp).
"""

import sys, os, math, string
from simviz1cp_unit import SimViz
# Note: scitools must be installed or available in sys.path
# can of course pack it out under src/tools:
sys.path.insert(0, os.path.join(os.pardir, os.pardir, 
                                os.pardir, 'tools'))
from scitools.TkGUI import Parameters, AutoSimVizCGI
import cgi

class SimVizCGI(SimViz):
    def __init__(self):
        self.cwd = os.getcwd()        
        self.form = cgi.FieldStorage()
        self.p = Parameters(interface='CGI', form=self.form)
        self.initialize()

        self.CGI = AutoSimVizCGI()

        self.CGI.make(
            self.form,
            self.p,
            # name of this (ACTION) script
            'wrapper.sh.cgi?s=simviz1cpCGI_unit.py.cgi',
            imagefile=os.path.join(os.pardir,os.pardir,os.pardir,
                                   'misc','figs','simviz.xfig.gif'),
            )

        self.simulate_and_visualize()
        self.CGI.footer()

    def simulate_and_visualize(self):
        # make sure that simviz1.py finds the oscillator code and the
        # gnuplot program, i.e., define absolute path to these codes
        # and add to PATH:
        root = '/hom/inf3330/www_docs/'
        osc = root + 'scripting/SunOS/bin'
        gnuplot = root + 'packages/SunOS/bin'
        other = '/local/bin:/usr/bin:/bin'
        os.environ['PATH'] = string.join([os.environ['PATH'],
                                          osc,gnuplot,other],':')

        # consistency checks:
        if not os.access(os.curdir, os.W_OK):
            print 'Current directory has not write permissions '\
                  'so it is impossible to perform simulations'

        # extra check that oscillator and gnuplot are really found:
        from py4cs.misc import pathsearch
        pathsearch(['oscillator','gnuplot'],[])  

        # do not run simulations if the form is not filled out:
        if not self.form:
            return

        self.simulate()

        # make sure we don't launch a plot window (may crash the script
        # when run in a browser):
        self.p['screenplot'] = 0
        
        self.visualize()

        # write HTML code for displaying a curve:
        case = self.p['case']
        imgfile = os.path.join(case+'.png')  # (we are in subdir)
        if os.path.isfile(imgfile):
            # make an arbitrary new filename to prevent that browsers
            # may reload the image from a previous run:
            import random
            newimgfile = 'tmp_'+str(random.uniform(0,2000))+'.png'
            os.rename(imgfile, newimgfile)
            print '<IMG SRC="%s/%s">' % (case,newimgfile)
        
        
if __name__ == '__main__':
    try:
        import cgitb; cgitb.enable()
    except:
        # older Python version
        sys.stderr = sys.stdout
    print 'Content-type: text/html\n'
    c = SimVizCGI()
