#!/usr/bin/env python

# Created by Ilmar M. Wilbers, 2009.
#
# First added:  2009-05-13
# Last changed: 2009-05-13

import sys, os, numpy

class Plotter(object):
    """
    Base class for plotting data from a Reader object.
    """
    def __init__(self, reader=None):
        if isinstance(reader, Reader):
            self.reader = reader

    def plot(self, reader=None):
        if reader:
            self.reader = reader
        assert self.reader is not None
        data = self.reader.data()
        self.backend.plot(data[0], data[1])   

class EasyvizPlotter(Plotter):
    """
    Derived class for plotting with Easyviz.
    We only import easyviz, and inherit the plot function from the base class.
    """
    def __init__(self, reader=None):
        Plotter.__init__(self, reader)
        from scitools import easyviz
        self.backend = easyviz

class MatplotlibPlotter(Plotter):
    """
    Derived class for plotting with Matplotlib.
    We use most the plot function from the base class, but add more code.
    """
    def __init__(self, reader=None):
        Plotter.__init__(self, reader)
        import pylab
        self.backend = pylab

    def plot(self, reader=None):
        Plotter.plot(self, reader)
        self.backend.show()

class GnuplotPlotter(Plotter): 
    """
    Derived class for plotting with Gnuplot.
    We don't use the polt function from the base class, but write out own.
    """
    def __init__(self, reader=None):
        Plotter.__init__(self, reader)
        import Gnuplot
        self.backend = Gnuplot

    def plot(self, reader=None):
        if reader:
            self.reader = reader
        assert self.reader is not None
        data = self.reader.data()
        be = self.backend
        gp = be.Gnuplot(persist=1)
        gp.plot(be.Data(data[0], data[1], with_='lines'))


class Reader(object): 
    """
    Base class for reading data.
    """
    def __init__(self, filename):
        self.filename = filename
        self.readdata()

    def readdata(self):
        raise NotImplementedError
        
    def data(self):
        return self.x, self.y

class AsciiReader(Reader):
    """
    Derived class for reading data from plain ASCII file.
    Assumes to floats per line, corresponding to the x and y values.
    """
    def readdata(self, fileobj=None):
        self.x = []; self.y = []
        if not fileobj:
            fileobj = open(self.filename)
        for line in fileobj:
            x, y = line.split()
            self.x.append(float(x))
            self.y.append(float(y))
        fileobj.close()

class BinaryReader(Reader):
    """
    Derived class for reading data from binary file.
    Assumes file contains two arrays written with numpy's ndarray.tolist()
    """
    def readdata(self):
        ifile = open(self.filename, 'rb')
        xy = numpy.fromfile(ifile)
        self.x = xy[:len(xy)/2]
        self.y = xy[len(xy)/2:]
        ifile.close()

class ZipReader(AsciiReader):
    """
    Derived class for reading data from compressed files.
    Assumes a plain ASCII file that is compressed using gzip.
    """
    def readdata(self):
        import gzip
        fileobj = gzip.open(self.filename)
        AsciiReader.readdata(self, fileobj)

class WebReader(ZipReader, BinaryReader):
    """
    Derived class for reading data from the internet.
    Assumes a URL containing a plain ASCII file.
    """
    def __init__(self, filename, binary=False):
        self.binary = binary
        Reader.__init__(self, filename)
                
    def readdata(self):
        import urllib
        urlobject = urllib.urlretrieve(self.filename)
        self.filename = urlobject[0]
        extension = os.path.splitext(self.filename)[1]
        if extension[1:] == 'gz':
            ZipReader.readdata(self)
        elif self.binary:
            BinaryReader.readdata(self)
        else:
            AsciiReader.readdata(self)

def _test():
    readers = []
    readers.append(AsciiReader('test.txt'))
    readers.append(BinaryReader('test.bin'))
    readers.append(ZipReader('test.txt.gz'))
    readers.append(WebReader('http://folk.uio.no/ilmarw/tmp/test.txt'))
    readers.append(WebReader('http://folk.uio.no/ilmarw/tmp/test.bin', True))
    readers.append(WebReader('http://folk.uio.no/ilmarw/tmp/test.txt.gz'))
    for reader in readers:
        for plotter in Plotter.__subclasses__():
            p = plotter(reader)
            p.plot()
        
if __name__ == "__main__":
    _test()
