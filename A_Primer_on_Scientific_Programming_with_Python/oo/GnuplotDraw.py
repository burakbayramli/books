import os

class GnuplotDraw:
    line_colors = {'red': 1, 'green': 2, 'blue': 3, 'cyan': 5,
                   'magenta': 4, 'aqua': 5, 'purple': 4,
                   'yellow': 7, 'black': -1}
    
    def __init__(self, xmin, xmax, ymin, ymax, axis=False,
                 instruction_file=None):
        """Define the drawing area [xmin,xmax]x[ymin,ymax]."""
        self.xmin, self.xmax, self.ymin, self.ymax = \
             float(xmin), float(xmax), float(ymin), float(ymax)
        self.axis = axis

        # Compute the right X11 geometry on the screen based on the
        # x-y ratio of axis ranges
        ratio = (self.ymax-self.ymin)/(self.xmax-self.xmin)
        self.xsize = 500
        self.ysize = 500*ratio
        geometry = '%dx%d' % (self.xsize, self.ysize)
        self.g = os.popen('gnuplot -geometry %s -persist' % geometry, 'w')

        if instruction_file is not None:
            # Collect all commands in __call__ in a file
            self.instruction_file = open(instruction_file, 'w')
        else:
            self.instruction_file = None
            
        if not axis:
            self("unset xtics; unset ytics\n")
            
        self("set xrange[%g:%g]; set yrange[%s:%s]\n" %
             (xmin, xmax, ymin, ymax))
        self("set size square")  # equal aspect ratio

        self.erase()
        self.file_counter = 0

        self.set_linecolor('red')
        self.set_linewidth(2)
        self.filled_curves(False)

    def set_linecolor(self, color):
        """Change the color of lines."""
        self.linecolor = GnuplotDraw.line_colors[color]

    def set_linewidth(self, width):
        """Change the line width (int, starts at 1)."""
        self.linewidth = width

    def filled_curves(self, on=True):
        """Fill area inside curves with current line color."""
        self.filledcurves = on
        
    def erase(self):
        """Erase the current figure."""
        # Don't set self.counter=0 here because that will overwrite
        # older files being plotted by Gnuplot.
        self.plot_commands = []

    def define_curve(self, x, y):
        """Define a curve with coordinates x and y (arrays)."""
        self.file_counter += 1
        filename = '.tmp_%04d' % self.file_counter
        f = open(filename, 'w')
        for xi, yi in zip(x, y):
            f.write('%g %g\n' % (xi, yi))
        f.close()
        if self.filledcurves:
            with_value = 'filledcurves'
        else:
            with_value = 'lines'
        self.plot_commands.append('"%s" with %s title "" lt %s lw %s' % 
            (filename, with_value, self.linecolor, self.linewidth))

    def display(self):
        """Display the figure."""
        if not self.plot_commands:
            return
        
        # The set output command is important because if hardcopy is
        # called and output is set to a file, a new set term x11 will
        # overwrite the file with empty content (see the resulting
        # set of commands to realize how PNG and x11 plots are made
        # in sequence).
        plotcommand = 'set output; set term x11; plot ' + \
                      ', '.join(self.plot_commands) + '\n'
        self(plotcommand)
        #self('pause 2\n')

    def hardcopy(self, name):
        """Save figure in PNG file name.png."""
        # Ratio is preserved by setting size 10cm,10cm for example
        # (at least this works for postscript)

        # Important: just running replot will not work if not display
        # is called so we make the full plot command here.
        plotcommand = 'plot ' + ', '.join(self.plot_commands) + '\n'

        self("""
set term png small size %d,%d
set output "%s.png"
%s
"""% (self.xsize, self.ysize, name, plotcommand))

    def write_text(self, text, position, alignment='center'):
        """
        Write text at a position (centered, left, right - according
        to the alignment string). position is a 2-tuple.
        """
        self('set label "%s" at %s,%s %s font "Helvetica,28" front; replot\n' %
             (text, position[0], position[1], alignment))

    def length_symbol(self, start, stop, symbol):
        #Divide arrow into two pieces, a textbox in the middle.
        #set arrow from 0,0 to 2,3 linewidth 1 heads
        #set label "..." at x,y center rotate by <degrees> font "Times,18" front
        pass
        
    def __call__(self, command):
        if self.instruction_file is not None:
            self.instruction_file.write(command)
        self.g.write(command)
        self.g.flush()

