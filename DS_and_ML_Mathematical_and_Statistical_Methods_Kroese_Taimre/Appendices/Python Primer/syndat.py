""" syndat.py """
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split

np.random.seed(1234)
X=np.pi*(2* np.random.random (size =(400 ,2)) -1)
y=(np.cos(X[: ,0])*np.sin(X[: ,1]) >=0)
X_train , X_test , y_train , y_test = train_test_split(X, y, test_size =0.5)

fig = plt.figure()
ax = fig.add_subplot(111)
ax.scatter( X_train [ y_train ==0 ,0] , X_train [ y_train ==0 ,1] , c='g',
marker ='o',alpha =0.5)
ax.scatter ( X_train [ y_train ==1 ,0] , X_train [ y_train ==1 ,1] , c='b',
marker ='o',alpha =0.5)
ax.scatter ( X_test [ y_test ==0 ,0] , X_test [ y_test ==0 ,1] , c='g',
marker ='s',alpha =0.5)
ax.scatter ( X_test [ y_test ==1 ,0] , X_test [ y_test ==1 ,1] , c='b',
marker ='s',alpha =0.5)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plt.savefig('sklearntraintest.pdf ',format ='pdf')
plt.show()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%