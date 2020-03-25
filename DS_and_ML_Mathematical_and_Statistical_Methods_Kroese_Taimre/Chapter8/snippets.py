""" snippets.py """ 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
from sklearn.datasets import make_blobs
X, y =  make_blobs(n_samples=5000, n_features=10, centers=3,  random_state=10, cluster_std=10)	
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
import numpy as np
from sklearn.datasets import make_friedman1
from sklearn.tree import DecisionTreeRegressor
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score
# create regression problem
n_points = 1000 # points
x, y = make_friedman1( n_samples =n_points , n_features =15 ,
                          noise =1.0 , random_state =100)

# split to train /test set
x_train , x_test , y_train , y_test = \
train_test_split (x, y, test_size =0.33 , random_state =100)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
