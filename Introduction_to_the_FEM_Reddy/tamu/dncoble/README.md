
Issues

Gives errors

```
fem2d.py:311: VisibleDeprecationWarning: Creating an ndarray from ragged nested sequences (which is a list-or-tuple of lists-or-tuples-or ndarrays with different lengths or shapes) is deprecated. If you meant to do this, you must specify 'dtype=object' when creating the ndarray
  prim_mat = np.array(m)
fem2d.py:322: VisibleDeprecationWarning: Creating an ndarray from ragged nested sequences (which is a list-or-tuple of lists-or-tuples-or ndarrays with different lengths or shapes) is deprecated. If you meant to do this, you must specify 'dtype=object' when creating the ndarray
  sec_mat = np.array(m)
Traceback (most recent call last):
  File "example_torsion.py", line 67, in <module>
    mesh.postprocess(poly_order=2)
  File "fem2d.py", line 324, in postprocess
    prim_mat = prim_mat[:,1:]
IndexError: too many indices for array: array is 1-dimensional, but 2 were indexed
```
