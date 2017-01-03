import numpy as np
# Utilities for axis labels in LaTeX with units in \rm font
label_magnitude_unit = lambda lab,mag,unit: (
    r'$%s/(10^{%d}\ {\rm{%s}})$'%(lab,mag,unit))
label_unit = lambda lab,unit: r'$%s/{\rm{%s}}$'%(lab,unit)
label_magnitude = lambda lab,mag: r'$%s/10^{%d}$'%(lab,mag)
magnitude = lambda A: int(np.log10(np.abs(A).max()))

class axis(object):
    ''' Class for managing scaling and labeling 1-d axes and data.
    '''
    def __init__(self,        # axis
                 **kwargs):   # Any keyword argument is legal
        ''' Hides some logic that figures out how to format axis
        labels, ticks and tick labels.
        '''
        self.__dict__.update(kwargs)
        defaults = dict(
            data=None,         # np array
            magnitude='auto',  # Power of 10.  False to suppress
            ticks='auto',      # np array.  False to suppress ticks
            label=False,       # string, eg 'force'
            units=None,        # eg, 'dyn'
            tick_label_flag=True)
        for key,value in defaults.items():
            if not key in self.__dict__:
                self.__dict__[key] = value
        if self.magnitude == 'auto' and isinstance(self.data,np.ndarray):
            self.magnitude = magnitude(self.data)
        if self.label == False:
            return
        # Calculate label string
        M = not (self.magnitude == 0 or self.magnitude == False)
        U = isinstance(self.units,str)
        self.label = {
         (True,True):label_magnitude_unit(self.label,self.magnitude,self.units),
         (True,False):label_unit(self.label,self.units),
         (False,True):label_magnitude(self.label,self.magnitude),
         (False,False):r'$%s$'%(self.label,)
            }[(U,M)]
        return
    def get_data(self,):
        if isinstance(self.magnitude,int):
            return self.data/10**self.magnitude
        return self.data
    def set_label(self,func):
        '''Apply func, eg, mpl.axis.set_xlabel(), to self._label
        '''
        if self.label != False:
            func(self.label)
        return
    def set_ticks(self,tick_func,label_func):
        '''Apply functions, eg, mpl.axis.set_xticks() and
        mpl.axis,set_xticklabels() to self.ticks
        '''
        if self.tick_label_flag == False:
            label_func([])
        if isinstance(self.ticks,str) and self.ticks == 'auto':
            return
        else:
            tick_func(self.ticks,minor=False)
        if self.tick_label_flag:
            if np.abs(self.ticks - self.ticks.astype(int)).sum() == 0:
                label_func([r'$%d$'%int(f) for f in self.ticks])
            else:
                label_func([r'$%1.1f$'%f for f in self.ticks])
        return
def SubPlot(fig,position,x,y,plot_flag=True,label=None,color='b'):
    ''' Make a subplot for fig using data and format in axis objects x and y
    '''
    ax = fig.add_subplot(*position)
    if plot_flag:
        if x.data == None:
            ax.plot(y.get_data(),color=color,label=label)
        else:
            ax.plot(x.get_data(),y.get_data(),color=color,label=label)
    y.set_label(ax.set_ylabel)
    y.set_ticks(ax.set_yticks,ax.set_yticklabels)
    x.set_label(ax.set_xlabel)
    x.set_ticks(ax.set_xticks,ax.set_xticklabels)
    return ax

def read_data(data_file):
    '''Read in "data_file" as an array'''
    f = file(data_file, 'r')
    data = [[float(x) for x in line.split()] for line in f.xreadlines()]
    f.close()
    return np.array(data).T

#---------------
# Local Variables:
# eval: (python-mode)
# End:
