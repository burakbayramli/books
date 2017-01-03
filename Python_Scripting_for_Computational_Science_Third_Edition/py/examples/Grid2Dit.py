#!/usr/bin/env python

from scitools.numpyutils import *
import time, sys, Gnuplot, os
# make sys.path so we can find Grid2D.py:
sys.path.insert(0, os.path.join(os.environ['scripting'],
                                'src','py','examples'))
from Grid2D import Grid2D

# iterator domains:
INTERIOR=0; BOUNDARY=1; CORNERS=2; ALL=3
# boundary parts:
RIGHT=0; UPPER=1; LEFT=2; LOWER=3

class Grid2Dit(Grid2D):
    def interior(self):
        self._iterator_domain = INTERIOR
        return self
    
    def boundary(self):
        self._iterator_domain = BOUNDARY
        return self

    def corners(self):
        self._iterator_domain = CORNERS
        return self

    def all(self):
        self._iterator_domain = ALL
        return self

    def __iter__(self):
        if self._iterator_domain == INTERIOR:
            self._i = 1; self._j = 1
        elif self._iterator_domain == BOUNDARY:
            self._i = len(self.xcoor)-1; self._j = 1
            self._boundary_part = RIGHT
        elif self._iterator_domain == CORNERS:
            nx = len(self.xcoor)-1;  ny = len(self.ycoor)-1
            self._corners = ((0,0), (nx,0), (nx,ny), (0,ny))
            self._corner_index = 0
        elif self._iterator_domain == ALL:
            self._i = 0; self._j = 0
        return self

    def next(self):
        if self._iterator_domain == INTERIOR:
            return self._next_interior()
        elif self._iterator_domain == BOUNDARY:
            return self._next_boundary()
        elif self._iterator_domain == CORNERS:
            return self._next_corners()
        elif self._iterator_domain == ALL:
            return self._next_all()

    def _next_interior(self):
        """Return the next interior grid point."""
        nx = len(self.xcoor)-1;  ny = len(self.ycoor)-1
        if self._i >= nx:
            # start on a new row:
            self._i = 1;  self._j += 1
        if self._j >= ny:
            raise StopIteration # end of last row
        item = (self._i, self._j)
        self._i += 1  #  walk along rows...
        return item

    def _next_all(self):
        """Return the next grid point."""
        nx = len(self.xcoor)-1;  ny = len(self.ycoor)-1
        # as _next_interior, but larger i,j limits
        if self._i > nx:
            # start on a new row:
            self._i = 0;  self._j += 1
        if self._j > ny:
            raise StopIteration # end of last row
        item = (self._i, self._j)
        self._i += 1
        return item

    def _next_boundary(self):
        """Return the next boundary point."""
        nx = len(self.xcoor)-1;  ny = len(self.ycoor)-1
        if self._boundary_part == RIGHT:
            if self._j < ny:
                item = (self._i, self._j)
                self._j += 1  # move upwards
            else: # switch to next boundary part:
                self._boundary_part = UPPER
                self._i = 1;  self._j = ny
        if self._boundary_part == UPPER:
            if self._i < nx:
                item = (self._i, self._j)
                self._i += 1  # move to the right
            else: # switch to next boundary part:
                self._boundary_part = LEFT
                self._i = 0;  self._j = 1
        if self._boundary_part == LEFT:
            if self._j < ny:
                item = (self._i, self._j)
                self._j += 1  # move upwards
            else: # switch to next boundary part:
                self._boundary_part = LOWER
                self._i = 1;  self._j = 0
        if self._boundary_part == LOWER:
            if self._i < nx:
                item = (self._i, self._j)
                self._i += 1  # move to the right
            else: # end of (interior) boundary points:
                raise StopIteration
        return item

    def _next_corners(self):
        """Return the next corner point."""
        if self._corner_index < len(self._corners):
            item = self._corners[self._corner_index]
            self._corner_index += 1
            return item
        else:
            raise StopIteration
                                
class Grid2Ditv(Grid2Dit):
    """Vectorized version of Grid2Dit."""
    def __iter__(self):
        # Fill self._indices with a list of (imin,imax,jmin,jmax)
        # tuples. The usage is typically as in
        #          somearray[imin:imax,jmin:jmax] = ...
        # so imax and jmax must be one larger than the physical limit
        nx = len(self.xcoor)-1;  ny = len(self.ycoor)-1
        if self._iterator_domain == INTERIOR:
            self._indices = [(1,nx, 1,ny)]
        elif self._iterator_domain == BOUNDARY:
            self._indices = [(nx,nx+1, 1,ny),
                             (1,nx, ny,ny+1),
                             (0,1, 1,ny),
                             (1,nx, 0,1)]
        elif self._iterator_domain == CORNERS:
            self._indices = [(0,1, 0,1),
                             (nx, nx+1, 0,1),
                             (nx,nx+1, ny,ny+1),
                             (0,1, ny,ny+1)]
        elif self._iterator_domain == ALL:
            self._indices = [(0,nx+1, 0,ny+1)]
        self._indices_index = 0
        return self

    def next(self):
        if self._indices_index <= len(self._indices)-1:
            item = self._indices[self._indices_index]
            self._indices_index += 1
            return item
        else:
            raise StopIteration

def _verify1_Grid2Dit():
    grid = Grid2Dit(dx=1.0, dy=1.0, xmax=2.0, ymax=2.0)
    def printpoint(text, i, j):
        """Print a point's indices (i,j) and coordinates."""
        print text, '(%d,%d): (%g,%g)' % \
              (i,j,grid.xcoor[i],grid.ycoor[j])

    for i, j in grid.interior():
        printpoint('interior point', i, j)

    for i, j in grid.boundary():
        printpoint('boundary point', i, j)

    for i, j in grid.corners():
        printpoint('corner point', i, j)

    for i, j in grid.all():  # visit all points
        printpoint('any grid point', i, j)

def _verify1_Grid2Ditv():
    grid = Grid2Ditv(dx=1.0, dy=1.0, xmax=2.0, ymax=2.0)
    def printpoint(intro, imin, imax, jmin, jmax):
        """Print grid point slices and corresponding coordinates."""
        print intro, '[%d:%d,%d:%d]' % (imin,imax,jmin,jmax)

    for pt_tp in ('interior', 'boundary', 'corners', 'all'):
        for imin,imax, jmin,jmax in getattr(grid, pt_tp)():
            printpoint('%s points' % pt_tp, imin,imax, jmin,jmax)

if __name__ == '__main__':
    try:
        func = sys.argv[1]
    except:
        func = 'verify1'
    exec func + '()'

