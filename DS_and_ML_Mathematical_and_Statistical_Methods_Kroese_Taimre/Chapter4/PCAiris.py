""" PCAiris.py """
import seaborn as sns, numpy as np

X = np.genfromtxt('irisX.csv',delimiter=',')
n = X.shape[0]
X = X - np.mean(X, axis=0)

U, D2, UT = np.linalg.svd((X.T @ X)/n)
print('U = \n', U); print('\n diag(D^2) = ', D2)

z =  U[:,0].T @ X.T
    
sns.kdeplot(z, bw=0.15)