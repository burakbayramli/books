#!/usr/bin/env python
"""
Sparse vectors implemented via a dictionary.
"""

class SparseVec:
    def __init__(self, length, nonzeroes={}):
        self.n = length
        self.vec = nonzeroes

    def __getitem__(self, index):
        return self.vec.get(index, 0.0)

    def __setitem__(self, index, value):
        if not isinstance(index, int):
            raise TypeError, 'index (%s) must be an integer' % \
                  str(index)
        if index >= self.n:
            raise IndexError, \
                  'index (%d) out of bounds (0:%d)' % \
                  (index, self.n-1)
        if isinstance(value, (float, int, long)):
            self.vec[index] = float(value)
        else:
            raise ValueError, 'only numbers can be assigned'

    def __add__(self, other):
        for index in self.other.keys():
            if index in self.vec:
                self.vec[index] += other.vec[index]
            else:
                self.vec[index] = other.vec[index]
                
    def __len__(self):  return self.n

    def __repr__(self):
        """complete representation; inverse of eval"""
        return 'SparseVec(%d, %s)' % (self.n, str(self.vec))
    
    def __str__(self):
        s = 'SparseVec(%d)' % self.n + ': {'
        # print sorted indices (keys):
        indices = self.vec.keys()
        indices.sort()
        for index in indices:
            s += str(index) + ': ' + str(self.vec[index])
        s += '}'
        return s

    # could define iterator (and use it in __add__?)
    # better example (this goes to ch 5 anyway) with sparse matrix?
    # (i,j) tuples , can exemplify matrix-vector product
        
        

if __name__ == '__main__':
    v=SparseVec(100)
    v[1]=0.1
    v[5]=2.2
    print 'v[1]=%g v[7]=%g' % (v[4],v[5])
    try:
        v[200] = 0.1
    except IndexError, e:
        print 'v[200] = 0.1;', e
    try:
        v[7] = (1,2)
    except ValueError, e:
        print 'v[7] = (1,2);', e
    
    print v
