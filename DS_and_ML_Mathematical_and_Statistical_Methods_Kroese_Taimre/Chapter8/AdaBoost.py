""" AdaBoost.py """
from sklearn.datasets import make_blobs
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import zero_one_loss
import numpy as np


def ExponentialLoss(y,yhat):
    n = len(y)
    loss = 0
    for i in range(n):
        loss = loss+np.exp(-y[i]*yhat[i])
    loss = loss/n
    return loss

# create binary classification problem
np.random.seed(100)

n_points = 100 # points
x, y =  make_blobs(n_samples=n_points, n_features=5,  centers=2,
                      cluster_std=20.0, random_state=100)
y[y==0]=-1  

# AdaBoost implementation
BoostingRounds = 1000
n = len(x)
W = 1/n*np.ones(n)

Learner = []
alpha_b_arr = []

for i in range(BoostingRounds):
    clf = DecisionTreeClassifier(max_depth=1)
    clf.fit(x,y, sample_weight=W)
    
    Learner.append(clf)
    
    train_pred = clf.predict(x)
    err_b = 0
    for i in range(n):
        if(train_pred[i]!=y[i]):
            err_b = err_b+W[i]
    err_b = err_b/np.sum(W)
        
    alpha_b = 0.5*np.log((1-err_b)/err_b)
    
    alpha_b_arr.append(alpha_b)
    
    for i in range(n):
        W[i] = W[i]*np.exp(-y[i]*alpha_b*train_pred[i])        
    
yhat_boost = np.zeros(len(y))

for j in range(BoostingRounds):
    yhat_boost = yhat_boost+alpha_b_arr[j]*Learner[j].predict(x)
    
    
yhat = np.zeros(n)
yhat[yhat_boost>=0]=1
yhat[yhat_boost<0]=-1
print("AdaBoost Classifier exponential loss = ", ExponentialLoss(y, yhat_boost)) 
print("AdaBoost Classifier zero-one loss = ", zero_one_loss(y,yhat) ) 
     
    



