""" VarImportance.py """

import numpy as np
from sklearn.datasets import make_classification
from sklearn.ensemble import RandomForestClassifier
import matplotlib.pyplot as plt, pylab

# create regression problem
n_points = 1000 # points
x, y =  make_classification(n_samples=n_points, n_features=15, n_informative=5,
                            n_redundant=0, n_repeated=0, random_state=100, shuffle=False)

rf = RandomForestClassifier(n_estimators=200, max_features  = "log2")

rf.fit(x,y)


importances = rf.feature_importances_
indices = np.argsort(importances)[::-1]

for f in range(15):
    print("Feature %d (%f)" % (indices[f]+1, importances[indices[f]]))


std = np.std([rf.feature_importances_ for tree in rf.estimators_],
             axis=0)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f = plt.figure()
#plt.title("Feature importances")
plt.bar(range(x.shape[1]), importances[indices],
       color="b", yerr=std[indices], align="center")
plt.xticks(range(x.shape[1]), indices+1)
plt.xlim([-1, x.shape[1]])
pylab.xlabel("feature index")
pylab.ylabel("importance")
plt.show()
f.savefig("varimport.pdf", bbox_inches='tight')
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%