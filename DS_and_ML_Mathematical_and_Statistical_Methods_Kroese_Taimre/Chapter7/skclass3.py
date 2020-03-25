""" skclass3.py """
from skclass1 import X_train, y_train, X_test, y_test
from sklearn.metrics import accuracy_score

import sklearn.discriminant_analysis as DA
from sklearn.naive_bayes import GaussianNB
from sklearn.neighbors import KNeighborsClassifier
#from sklearn.neighbors import DistanceMetric
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC
#import numpy as np
#from sklearn.preprocessing import MinMaxScaler

names = ["Logit","NBayes", "LDA", "QDA", "KNN", "SVM"]
#%% 
classifiers = [LogisticRegression(C=1e5),
               GaussianNB(), 
               DA.LinearDiscriminantAnalysis(),
               DA.QuadraticDiscriminantAnalysis(), 
               KNeighborsClassifier(n_neighbors=5),
               SVC(kernel='rbf', gamma = 1e-4)]
             
print('Name  Accuracy\n'+14*'-') 
for name, clf in zip(names, classifiers):
    clf.fit(X_train, y_train)
    y_pred = clf.predict(X_test) 
    print('{:6}  {:3.3f}'.format(name, accuracy_score(y_test,y_pred)))