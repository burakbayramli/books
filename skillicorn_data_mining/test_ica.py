import mdp
import numpy as np

A = np.array(
    [[1, 2, 3, 4, 5, 6, 7, 8],
     [3, 4, 4, 5, 5, 6, 7, 9],
     [1, 8, 2, 7, 3, 6, 4, 5],
     [9, 8, 7, 6, 5, 4, 3, 2],
     [9, 4, 8, 3, 7, 2, 6, 1],
     [2, 3, 2, 4, 2, 5, 2, 6],
     [3, 4, 3, 4, 4, 3, 4, 3],
     [3, 2, 4, 3, 2, 4, 3, 2],
     [5, 5, 4, 4, 6, 6, 2, 2],
     [2, 3, 6, 5, 4, 6, 7, 2],
     [1, 6, 5, 3, 8, 2, 3, 9]])
     
# perform ICA on some data x using single precision
y = mdp.fastica(A, dtype='float32') 
print y
