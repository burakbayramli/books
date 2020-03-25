""" skclass1.py """
from numpy import genfromtxt
from sklearn.model_selection import train_test_split
url = "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/"
name = "wdbc.data"
data = genfromtxt(url + name,delimiter=',', dtype=str)
y = data[:,1] #responses 
X = data[:,2:].astype('float')#features as an ndarray matrix

X_train , X_test , y_train , y_test = train_test_split(
        X, y, test_size = 0.4, random_state = 1234)