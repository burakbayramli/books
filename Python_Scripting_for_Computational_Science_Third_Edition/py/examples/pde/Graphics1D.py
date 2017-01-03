#!/usr/bin/env python

class Graphics1D:
    def __init__(self):
        self.plot_prm = {
            'graphics': True,
            'program': 'Gnuplot',
            'xcoor': linspace(0,1,101),
            'ymin': 0,
            'ymax': 1,
            'plotskip': 1,
            'mpeg': False,
            'plot_times': None,
            'hardcopy': 0,  # or 'ps'
            }
        self.graphics = None

    def set(self, **kwargs):
        for key in kwargs:
            if key in self.plot_prm:
                self.plot_prm[key] = kwargs[key]
        self._update()

    def _update(self):
        """Update internal data structures."""
        G = self.plot_prm   # short form
        if G['graphics']:
            if self.graphics is None:
                self.graphics = graph(program=G['program'])
            self.graphics.configure(\
                coor=G['xcoor'], ymin=G['ymin'], ymax=G['ymax'])

    def grid(self, x):
        self.plot_prm['xcoor'] = x
        if self.plot_prm['graphics']:
            self.graphics.configure(coor=x)

    def do_plot(self, time, step_no=None):
        if self.plot_prm['plot_times'] is not None:
            return time in self.plot_prm['plot_times']  # approx...
        if step_no is not None:
            if step_no % plotskip == 0:
                return True
            else:
                return False
        return True

