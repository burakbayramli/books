from numpy import linspace, sin, cos, pi, asarray

drawing_toolkit = 'GnuplotDraw'
if drawing_toolkit == 'Easyviz':
    from scitools.std import *
    #did not work:
    Gnuplot.GnuplotOpts.gnuplot_command = 'gnuplot -geometry 100x100'
elif drawing_toolkit == 'GnuplotDraw':
    from GnuplotDraw import GnuplotDraw
else:
    raise NotImplementedError


class Shape:
    """
    Superclass for drawing different geometric shapes.
    Subclasses define shapes, but drawing, rotation, translation,
    etc. are done in generic functions in this superclass.
    """
    drawing_tool = None

    # All the static methods below are also mirrored in global functions
    
    @staticmethod
    def set_coordinate_system(xmin=0, xmax=1, ymin=0, ymax=1,
                              axis=False):
        # coordinate system:
        Shape.drawing_tool = GnuplotDraw(xmin, xmax, ymin, ymax,
                                         axis, '.tmp_gnuplot')

    @staticmethod
    def hardcopy(filename='tmp'):
        """Make a hardcopy in PNG format of the figure."""
        Shape.drawing_tool.hardcopy(filename)

    @staticmethod
    def erase():
        """Erase all elements in the figure."""
        Shape.drawing_tool.erase()

    @staticmethod
    def display():
        """
        Display all current instances from the Shape hierarchy
        on the screen. Each instance must have called
        its draw() method.
        """
        Shape.drawing_tool.display()

    @staticmethod
    def set_linecolor(color):
        """
        Set line color (string):
        red, green, blue, cyan/aqua, magenta/purple, yellow, black.
        """
        Shape.drawing_tool.set_linecolor(color)

    @staticmethod
    def set_linewidth(width):
        """Set line width (int, starting from 1)."""
        Shape.drawing_tool.set_linewidth(width)

    @staticmethod
    def filled_curves(on=True):
        """Fill space inside curves with current line color."""
        Shape.drawing_tool.filled_curves(on)


    def __init__(self):
        self.shapes = self.subshapes()
        if isinstance(self.shapes, Shape):
            self.shapes = [self.shapes]  # turn to list

        self._init_called = True

    def subshapes(self):
        """Define self.shapes as list of Shape instances."""
        raise NotImplementedError(self.__class__.__name__)

    def ok(self):
        if not hasattr(self, '_init_called'):
            print 'Forgot to call super class constructor in '\
                  'class %s?' % self.__class__.__name__
            return False
        else:
            return True
        
    def draw(self):
        for shape in self.shapes:
            if shape.ok():
                shape.draw()

    def rotate(self, angle, x=0, y=0):
        for shape in self.shapes:
            if shape.ok():
                shape.rotate(angle, x, y)

    def translate(self, x, y):
        for shape in self.shapes:
            if shape.ok():
                shape.translate(x, y)

    def scale(self, factor):
        for shape in self.shapes:
            if shape.ok():
                shape.scale(factor)


# For newcomers it can be handy to have global functions
# instead of static methods that require the Shape prefix.

staticmethods = 'set_coordinate_system', 'hardcopy', \
                'erase', 'display', 'set_linecolor', \
                'set_linewidth', 'filled_curves'
for method in staticmethods:
    cmd = '%s = Shape.%s' % (method, method)
    exec(cmd)

        
class Curve(Shape):
    """General (x,y) curve with coordintes."""
    def __init__(self, x, y):
        self.x, self.y = x, y
        # Turn to Numerical Python arrays
        self.x = asarray(self.x, float)
        self.y = asarray(self.y, float)
        Shape.__init__(self)

    def ok(self):
        return True
    
    def subshapes(self):
        pass # geometry defined in constructor

    def draw(self):
        """Easyviz implementation."""
        plot(self.x, self.y, 'r')
        axis([self.xmin, self.xmax, self.ymin, self.ymax])
        hold(True)

    def draw(self):
        Shape.drawing_tool.define_curve(self.x, self.y)

    def rotate(self, angle, x=0, y=0):
        """
        Rotate all coordinates: angle is measured in degrees and
        (x,y) is the "origin" of the rotation.
        """
        angle = angle*pi/180
        c = cos(angle);  s = sin(angle)
        xnew = x + (self.x - x)*c - (self.y - y)*s
        ynew = y + (self.x - x)*s + (self.y - y)*c
        self.x = xnew
        self.y = ynew

    def scale(self, factor):
        """Scale all coordinates by a factor."""
        self.x = factor*self.x
        self.y = factor*self.y

    def translate(self, x, y):
        """Translate all coordinates by a vector (x,y)."""
        self.x = x + self.x
        self.y = y + self.y


# Template class for a shape
class SomeShape(Shape):
    def __init__(self, arg1, arg2):
        # Store arg1, arg2 as attributes
        # when all data are stored.
        Shape.__init__(self)

    def subshapes(self):
        # Compute arrays of points, x and y, and/or Shape instances
        # and return tuple of Curve and/or Shape instances.
        x = [1,2,3]
        y = [4,5,6]
        return Curve(x,y)

        
class Line(Shape):
    def __init__(self, start, stop):
        self.start, self.stop = start, stop
        Shape.__init__(self)

    def subshapes(self):
        x = [self.start[0], self.stop[0]]
        y = [self.start[1], self.stop[1]]
        return Curve(x,y)

# First implementation of class Circle
class Circle(Shape):
    def __init__(self, center, radius, resolution=180):
        self.center, self.radius = center, radius
        self.resolution = resolution
        Shape.__init__(self)
        
    def subshapes(self):
        t = linspace(0, 2*pi, self.resolution+1)
        x0 = self.center[0];  y0 = self.center[1]
        R = self.radius
        x = x0 + R*cos(t)
        y = y0 + R*sin(t)
        return Curve(x,y)

class Arc(Shape):
    def __init__(self, center, radius,
                 start_degrees, opening_degrees, resolution=180):
        self.center = center
        self.radius = radius
        self.start_degrees = start_degrees*pi/180
        self.opening_degrees = opening_degrees*pi/180
        self.resolution = resolution
        Shape.__init__(self)
        
    def subshapes(self):
        t = linspace(self.start_degrees,
                     self.start_degrees + self.opening_degrees,
                     self.resolution+1)
        x0 = self.center[0];  y0 = self.center[1]
        R = self.radius
        x = x0 + R*cos(t)
        y = y0 + R*sin(t)
        return Curve(x,y)

class Circle(Arc):
    def __init__(self, center, radius, resolution=180):
        Arc.__init__(self, center, radius, 0, 360, resolution)

class Arrow(Shape):
    """Draw a vertical arrow (can be rotated later, if desired."""
    def __init__(self, bottom_point, length, rotation_angle=0):
        self.bottom = bottom_point
        self.length = length
        self.angle = rotation_angle
        Shape.__init__(self)
        # Must rotate after Shape.__init__ has called subshapes
        # and computed the elements of this shape:
        self.rotate(rotation_angle)

    def subshapes(self):
        top = (self.bottom[0], self.bottom[1] + self.length)
        main = Line(self.bottom, top)
        head_length = self.length/8.0
        head_degrees = 30*pi/180
        head_left = (top[0] - head_length*sin(head_degrees),
                     top[1] - head_length*cos(head_degrees))
        head_right = (top[0] + head_length*sin(head_degrees),
                     top[1] - head_length*cos(head_degrees))
        return [main, Line(head_left, top), Line(head_right, top)]
                      


class Wheel(Shape):
    def __init__(self, center, radius, inner_radius=None, nlines=10):
        self.center = center
        self.radius = radius
        if inner_radius is None:
            self.inner_radius = radius/5.0
        else:
            self.inner_radius = inner_radius
        self.nlines = nlines
        Shape.__init__(self)

    def subshapes(self):
        outer = Circle(self.center, self.radius)
        inner = Circle(self.center, self.inner_radius)
        lines = []
        # Draw nlines+1 since the first and last coincide
        # (then nlines lines will be visible)
        t = linspace(0, 2*pi, self.nlines+1)

        Ri = self.inner_radius;  Ro = self.radius
        x0 = self.center[0];  y0 = self.center[1]
        xinner = x0 + Ri*cos(t)
        yinner = y0 + Ri*sin(t)
        xouter = x0 + Ro*cos(t)
        youter = y0 + Ro*sin(t)
        lines = [Line((xi,yi),(xo,yo)) for xi, yi, xo, yo in \
                 zip(xinner, yinner, xouter, youter)]
        return [outer, inner] + lines

class Wave(Shape):
    def __init__(self, xstart, xstop,
                 wavelength, amplitude, mean_level):
        self.xstart = xstart
        self.xstop = xstop
        self.wavelength = wavelength
        self.amplitude = amplitude
        self.mean_level = mean_level
        Shape.__init__(self)

    def subshapes(self):
        npoints = (self.xstop - self.xstart)/(self.wavelength/61.0)
        x = linspace(self.xstart, self.xstop, npoints)
        k = 2*pi/self.wavelength # frequency
        y = self.mean_level + self.amplitude*sin(k*x)
        return Curve(x,y)


class Rectangle(Shape):
    def __init__(self, lower_left_corner, width, height):
        self.lower_left_corner = lower_left_corner  # 2-tuple
        self.width, self.height = width, height
        Shape.__init__(self)

    def subshapes(self):
        ll = self.lower_left_corner  # short form
        x = [ll[0], ll[0]+self.width,
             ll[0]+self.width, ll[0], ll[0]]
        y = [ll[1], ll[1], ll[1]+self.height,
             ll[1]+self.height, ll[1]]
        return Curve(x,y)

class Point:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def __add__(self, other):
        if isinstance(other, (list,tuple)):
            other = Point(other)
        return Point(self.x+other.x, self.y+other.y)
# make a version of Spring using Point class
    
    
class Spring(Shape):
    def __init__(self, bottom_point, length, tagwidth, ntags=4):
        """
        Specify a vertical spring, starting at bottom_point and
        having a specified lengths. In the middle third of the
        spring there are ntags tags.
        """
        self.B = bottom_point
        self.n = ntags - 1  # n counts tag intervals
        # n must be odd:
        if self.n % 2 == 0:
            self.n = self.n+1
        self.L = length
        self.w = tagwidth
        Shape.__init__(self)

    def subshapes(self):
        B, L, n, w = self.B, self.L, self.n, self.w  # short forms
        t = L/(3.0*n)  # must be better worked out
        P0 = (B[0], B[1]+L/3.0)
        P1 = (B[0], B[1]+L/3.0+t/2.0)
        P2 = (B[0], B[1]+L*2/3.0)
        P3 = (B[0], B[1]+L)
        line1 = Line(B, P1)
        lines = [line1]
        #line2 = Line(P2, P3)
        T1 = P1
        T2 = (T1[0] + w, T1[1] + t/2.0)
        lines.append(Line(T1,T2))
        T1 = (T2[0], T2[1])
        for i in range(n):
            T2 = (T1[0] + (-1)**(i+1)*2*w, T1[1] + t/2.0)
            lines.append(Line(T1, T2))
            T1 = (T2[0], T2[1])
        T2 = (T1[0] + w, T1[1] + t/2.0)
        lines.append(Line(T1,T2))

        #print P2, T2
        lines.append(Line(T2, P3))
        return lines


def _test1():
    set_coordinate_system(xmin=0, xmax=10, ymin=0, ymax=10)
    l1 = Line((0,0), (1,1))
    l1.draw()
    display()  # show the plot
    input(': ')
    c1 = Circle((5,2), 1)
    c2 = Circle((6,2), 1)
    w1 = Wheel((7,2), 1)
    c1.draw()
    c2.draw()
    w1.draw()
    display()  # show the plot
    hardcopy()

def _test2():
    set_coordinate_system(xmin=0, xmax=10, ymin=0, ymax=10)
    l1 = Line((0,0), (1,1))
    l1.draw()
    display()  # show the plot
    input(': ')
    c1 = Circle((5,2), 1)
    c2 = Circle((6,2), 1)
    w1 = Wheel((7,2), 1)
    filled_curves(True)
    set_linecolor('blue')
    c1.draw()
    set_linecolor('aqua')    
    c2.draw()
    filled_curves(False)
    set_linecolor('red')    
    w1.draw()
    display()  # show the plot
    hardcopy()

def _test3():
    """Test example from the book."""
    set_coordinate_system(xmin=0, xmax=10, ymin=0, ymax=10)
    l1 = Line(start=(0,0), stop=(1,1))  # define line
    l1.draw()        # make plot data
    display()        # display the plot data
    r1 = Rectangle(lower_left_corner=(0,1), width=3, height=5)
    r1.draw()
    display()
    Circle(center=(5,7), radius=1).draw()
    Wheel(center=(6,2), radius=2, inner_radius=0.5, nlines=7).draw()
    display()
    hardcopy()

def _test4():
    """Second example from the book."""
    set_coordinate_system(xmin=0, xmax=10, ymin=0, ymax=10)
    r1 = Rectangle(lower_left_corner=(0,1), width=3, height=5)
    c1 = Circle(center=(5,7), radius=1)
    w1 = Wheel(center=(6,2), radius=2, inner_radius=0.5, nlines=7)
    c2 = Circle(center=(7,7), radius=1)
    filled_curves(True)
    c1.draw()
    set_linecolor('blue')
    r1.draw()
    set_linecolor('aqua')
    c2.draw()
    # Add thick aqua line around rectangle:
    filled_curves(False)
    set_linewidth(4)
    r1.draw()
    set_linecolor('red')
    # Draw wheel with thick lines:
    w1.draw()
    display()
    hardcopy('tmp_colors')
    
    
def _test5():
    set_coordinate_system(xmin=0, xmax=10, ymin=0, ymax=10)
    c = 6.  # center point of box
    w = 2.  # size of box
    L = 3
    r1 = Rectangle((c-w/2, c-w/2), w, w)
    l1 = Line((c,c-w/2), (c,c-w/2-L))
    linecolor('blue')
    filled_curves(True)
    r1.draw()
    linecolor('aqua')
    filled_curves(False)
    l1.draw()
    display()  # show the plot
    hardcopy()

def rolling_wheel(total_rotation_angle):
    """Animation of a rotating wheel."""
    set_coordinate_system(xmin=0, xmax=10, ymin=0, ymax=10)

    import time
    center = (6,2)
    radius = 2.0
    angle = 2.0
    pngfiles = []
    w1 = Wheel(center=center, radius=radius, inner_radius=0.5, nlines=7)
    for i in range(int(total_rotation_angle/angle)):
        w1.draw()
        display()

        filename = 'tmp_%03d' % i
        pngfiles.append(filename + '.png')
        hardcopy(filename)
        time.sleep(0.3)  # pause

        L = radius*angle*pi/180  # translation = arc length
        w1.rotate(angle, center[0], center[1])
        w1.translate(-L, 0)
        center = (center[0] - L, center[1])

        erase()
    cmd = 'convert -delay 50 -loop 1000 %s tmp_movie.gif' \
          % (' '.join(pngfiles))
    print 'converting PNG files to animated GIF:\n', cmd
    import commands
    failure, output = commands.getstatusoutput(cmd)
    if failure:  print 'Could not run', cmd


if __name__ == '__main__':
    #rolling_wheel(40)
    #_test1()
    _test3()
    
