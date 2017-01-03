"""
Examples on general plotting with Gnuplot in
a separate thread.

Two methods for threads are exemplified:

 (i)  calling threading.Thread's constructor with the plot
      function as argument

 (ii) creating a class PlotThread as a subclass of threading.Thread
      with the plot function as a member function run and
      parameters transferred to the subclass constructor
"""

def plot(a, curve_label=''):
    """a is a list of [x,y] lists of data points."""
    import Gnuplot
    g = Gnuplot.Gnuplot()
    d = Gnuplot.Data(a, with='lines', title=curve_label)
    # let the Gnuplot object g plot the data object d:
    g.plot(d)
    g('pause 30')  # halt the plot for 30 seconds


import threading
class PlotThread(threading.Thread):
    def __init__(self, a, curve_label=''):
        "a is a list of [x,y] lists of data points"
        threading.Thread.__init__(self)
        self.a = a; self.curve_label = curve_label
    def run(self):
        import Gnuplot
        g = Gnuplot.Gnuplot()
        d = Gnuplot.Data(self.a, with='lines',
                         title=self.curve_label)
        # let the Gnuplot object g plot the data object d:
        g.plot(d)
        g('pause 30')  # display the plot for 30 seconds
