"""
Illustrate finite element function over 2D mesh with triangles.

Good set of parameters for f1 function:
nx = 4; ny = 3
view: 58, 345

"""

from fe_approx2D import mesh, np
import os

class Gnuplotter:
    def __init__(self):
        self.fc = 0  # file counter
        import io
        self.commands = io.StringIO()
        self._gnuwrite('unset border\nunset xtics\nunset ytics\nunset ztics\nset parametric\n')
        self._plot = []

    def _gnuwrite(self, command):
        import builtins
        builtins.unicode = str
        self.commands.write(unicode(command))

    def hold(self, mode):
        pass

    def plot3(self, x, y, z, style):
        if '--' in style or '..' in style:
            gnustyle = 'linespoints'
            lt = 2
        else:
            gnustyle = 'lines'
            lt = 1
        filename = '_tmp_%04d.dat' % self.fc
        self.fc += 1
        f = open(filename, 'w')
        for xi, yi, zi in zip(x, y, z):
            f.write('%g %g %g\n' % (xi, yi, zi))
        f.close()
        self._plot.append('"%s" using 1:2:3 with %s lt %d lw 2 title ""' %
                          (filename, gnustyle, lt))

    def view(self, angle1, angle2):
        self._gnuwrite('set view %d,%d\n' % (angle1, angle2))

    def axis(self, a):
        self._gnuwrite('set xrange [%g:%g]\nset yrange [%g:%g]\nset zrange [%g:%g]\n' % tuple(a))

    def savefig(self, filename):
        self._gnuwrite('splot %s\n' % ', '.join(self._plot))
        self._gnuwrite('set output "%s.eps"\n' % filename)
        self._gnuwrite('set terminal postscript eps enhanced monochrome\n')
        self._gnuwrite('replot\n')
        self._gnuwrite('set output "%s.png"\n' % filename)
        self._gnuwrite('set terminal png\n')
        self._gnuwrite('replot\n')

    def show(self):
        self._gnuwrite('replot\npause 3\n')
        text = self.commands.getvalue()
        f = open('_tmp.gnu', 'w')
        f.write(text)
        f.close()
        os.system('gnuplot _tmp.gnu')

class Matplotlibplotter:
    # http://matplotlib.org/examples/mplot3d/lines3d_demo.html
    pass



def f1(x, y):
    return 1 + 4*x*(1-x)*y + (1 - y)*(1-x**2)

def fill(f, vertices):
    values = np.zeros(vertices.shape[0])
    for i, point in enumerate(vertices):
        values[i] = f(point[0], point[1])
    return values

def draw_mesh(vertices, cells, plt, style='g--'):
    for local_vertices in cells:
        local_vertices = local_vertices.tolist()
        local_vertices.append(local_vertices[0])  # closed polygon
        x = [vertices[vertex,0] for vertex in local_vertices]
        y = [vertices[vertex,1] for vertex in local_vertices]
        z = [0 for vertex in local_vertices]
        plt.plot3(x, y, z, style)
        plt.hold('on')
    return

def draw_surface(zvalues, vertices, cells, plt, style='r-'):
    for local_vertices in cells:
        local_vertices = local_vertices.tolist()
        local_vertices.append(local_vertices[0])  # closed polygon
        x = [vertices[vertex,0] for vertex in local_vertices]
        y = [vertices[vertex,1] for vertex in local_vertices]
        z = [zvalues[vertex]    for vertex in local_vertices]
        plt.plot3(x, y, z, style)
        plt.hold('on')
    return

def demo1(f=f1, nx=4, ny=3, viewx=58, viewy=345, plain_gnuplot=True):
    vertices, cells = mesh(nx, ny, x=[0,1], y=[0,1], diagonal='right')
    if f == 'basis':
        # basis function
        zvalues = np.zeros(vertices.shape[0])
        zvalues[int(round(len(zvalues)/2.)) + int(round(nx/2.))] = 1
    else:
        zvalues = fill(f1, vertices)
    if plain_gnuplot:
        plt = Gnuplotter()
    else:
        import scitools.std as plt

    """
    if plt.backend == 'gnuplot':
    gpl = plt.get_backend()
    gpl('unset border; unset xtics; unset ytics; unset ztics')
    #gpl('replot')
    """
    draw_mesh(vertices, cells, plt)
    draw_surface(zvalues, vertices, cells, plt)
    plt.axis([0, 1, 0, 1, 0, zvalues.max()])
    plt.view(viewx, viewy)
    if plain_gnuplot:
        plt.savefig('tmp')
    else:
        plt.savefig('tmp.pdf')
        plt.savefig('tmp.eps')
        plt.savefig('tmp.png')
    plt.show()

#demo1()
demo1(f='basis', nx=4, ny=3, viewx=72)
