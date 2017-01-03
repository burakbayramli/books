#!/usr/bin/env python
    
class typedlist(list):
    """
    A list where each element has to be of the same type.

    >>> from typedlist import typedlist
    >>> q = typedlist()
    >>> q.append(19)     # restrict items to integers
    >>> q.append(8.8)
    Traceback (most recent call last):
    TypeError: items must be int, not float

    >>> class A: pass
    >>> class B: pass
    >>> q = typedlist()
    >>> q.append(A())
    >>> q.append(B())
    Traceback (most recent call last):
    TypeError: items must be A, not B

    >>> q = typedlist((1,4,3,2))
    >>> q.append((9,10))
    Traceback (most recent call last):
    TypeError: items must be int, not tuple
    >>> q = q + [9,2,3]
    >>> q
    [1, 4, 3, 2, 9, 2, 3]
    >>> q += [9.9,2,3]
    Traceback (most recent call last):
    TypeError: items must be int, not float

    >>> q = typedlist([1,A(),2])
    Traceback (most recent call last):
    TypeError: items must be int, not A
    """
    
    def __init__(self, somelist=[]):
        list.__init__(self, somelist)
        for item in self: self._check(item)
            
    def _check(self, item):
        if len(self) > 0:
            item0class = self.__getitem__(0).__class__
            if not isinstance(item, item0class):
                raise TypeError, 'items must be %s, not %s' \
                % (item0class.__name__, item.__class__.__name__)
            
    def __setitem__(self, i, item):
        self._check(item); list.__setitem__(self, i, item)
        
    def append(self, item):
        self._check(item); list.append(self, item)
        
    def insert(self, index, item):
        self._check(item); list.insert(self, index, item)

    def __add__(self, other):
        return typedlist(list.__add__(self, other))
    
    def __iadd__(self, other):
        return typedlist(list.__iadd__(self, other))
    # failing to return typedlist(...) here returns plain list
    # and the properties of typedlist are lost...

    def __setslice(self, slice, somelist):
        for item in somelist:  self._check(item)
        list.__setslice(self, slice, somelist)

    def extend(self, somelist):
        for item in somelist:  self._check(item)
        list.extend(self, somelist)

if __name__ == '__main__':
    q = typedlist()
    q.append(19)
    try:
        q.append(8.8)
    except TypeError, message:
        print 'detected wrong type;', message

    class A:
        pass
    class B:
        pass
    q = typedlist()
    q.append(A())
    try:
        q.append(B())
    except TypeError, message:
        print 'detected wrong type;', message

    q = typedlist((1,4,3,2))
    q = q + [9,2,3]
    q += [9,2,3]   # isn't this iadd?
    print q
    try:
        q += [9.9,2,3]
    except TypeError, message:
        print 'detected wrong type;', message

    try:
        q = typedlist([1,A(),2])
    except TypeError, message:
        print 'detected wrong type;', message

    # problem:
    q = [0.1]*10   # creates list...must say typedlist([0.1]*10) ok, reasonable!
    
