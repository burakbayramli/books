from shapes import *
import time

def draw(L, w, S, box_size, plotfilename=None):
    """Draw a box connected to a spring, connected to a plate."""
    Shape.erase()
    plate_position = w
    spring_length = L + S
    box_position = w - spring_length

    spring = Spring(bottom_point=(0,box_position),
                    length=spring_length, tagwidth=L*0.1, ntags=8)
    spring.draw()
    plate = Rectangle(lower_left_corner=(-box_size/2.0, w),
                      width=box_size, height=box_size*0.1)
    plate.draw()
    box = Rectangle(lower_left_corner=(-box_size/2.0,box_position-box_size),
                    width=box_size, height=box_size)
    box.draw()

    y0_line = Line((box_size/2.0 + L/10.0, 0), (box_size/2.0 + 2*L/10.0, 0))
    y0_line.draw()
    unstretched_spring_line = Line((box_size/2.0 + L/10.0, w-L),
                                   (box_size/2.0 + 2*L/10.0, w-L))
    unstretched_spring_line.draw()
    
    #Shape.drawing_tool.write_text('m', (box_size/2.0*1.1,box_position))
    #Shape.drawing_tool.write_text('plate w(t)', (box_size/2.0*1.1,w))
    Shape.display()
    if plotfilename is not None:
        Shape.hardcopy(plotfilename)

def set_figure_size(L, w_max, S_max, box_size):
    # Let y=0 correspond to position of the plate at rest.
    # Draw in physical coordinates.
    ymax_figure = w_max + L*0.1
    ymax = ymax_figure*1.2  # 20% space above highest plate position
    ymin_figure = w_max - L - S_max - box_size
    ymin = ymin_figure*1.2  # 20% space below figure
    xwidth = (ymax - ymin)
    xmin = -xwidth/2.0
    xmax =  xwidth/2.0
    Shape.set_coordinate_system(xmin=xmin, xmax=xmax,
                                ymin=ymin, ymax=ymax)
    return w_max, S_max

def oscillating_mass_dynamics(t, L, w, S, box_size):
    """
    Dynamic movement of the figure.
    t, w, and S are arrays with data for the time series.
    """
    w_max = max(w)
    S_max = max(S)
    set_figure_size(L, w_max, S_max, box_size)
    
    filenames = []
    for i in range(len(t)):
        filename = 'tmp_%04d' % i
        filenames.append(filename + '.png')
        draw(L, w[i], S[i], box_size, filename)
        #time.sleep(0.5)

    # Create movie
    if len(t) < 40:  # avoid too many frames
        time.sleep(1)  # wait for plot to be ready
        cmd = 'convert -delay 10 -loop 1000 %s tmp_movie.gif' \
              % (' '.join(filenames))
        print 'converting PNG files to animated GIF:\n', cmd
        import commands
        failure, output = commands.getstatusoutput(cmd)
        if failure:
            print output

def _test():
    def solution(t):
        return 1*cos(2*pi*t)

    import numpy as np
    t = np.linspace(0, 6, 120)
    S = 2*solution(t)
    w = np.zeros(len(t))
    oscillating_mass_dynamics(t, 10, w, S, 2)

if __name__ == '__main__':
    _test()
