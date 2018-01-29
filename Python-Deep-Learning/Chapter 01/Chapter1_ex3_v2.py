from sklearn.neural_network.multilayer_perceptron import MLPClassifier
from sklearn import datasets

# Since the book came out, the cross_validation method has been moved to
# the model_selection library from the cross_validation library
#from sklearn.cross_validation import train_test_split
from sklearn.model_selection import train_test_split
 
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import accuracy_score

import numpy
from matplotlib.colors import ListedColormap
import matplotlib.pyplot as plt

#Apply standardization
standardised = True

M = {0:"sepal length", 1:"sepal width", 2:"petal length", 3:"petal width"}

#Choose two features
x=1 #1 corresponds to the sepal width
y=3 #3 corresponds to the petal width

iris = datasets.load_iris()
data = iris.data[:,[x,y]]

labels = iris.target

X_train, X_test, y_train, y_test = train_test_split(data, labels, test_size=0.5, random_state=1)

reg = StandardScaler()
reg.fit(data)
X_train_std = reg.transform(X_train)
X_test_std = reg.transform(X_test)

if (standardised == False):
  X_train_std = X_train
  X_test_std = X_test

# We add max_iter=1000 becaue the default is max_iter=200 and 
# it is not enough for full convergence
mlp = MLPClassifier(random_state=1, max_iter=1000)
mlp.fit(X_train_std, y_train)

y_pred = mlp.predict(X_test_std)
print('Misclassified samples: %d' % (y_test != y_pred).sum())

print('Accuracy: %.2f' % accuracy_score(y_test, y_pred))


def plot_decision_regions(data, labels, classifier, resolution=0.01):
    markers = ('s', '*', '^')
    colors = ('blue', 'green', 'red')
    cmap = ListedColormap(colors)
    # plot the decision surface
    x_min, x_max = data[:, 0].min() - 1, data[:, 0].max() + 1
    y_min, y_max = data[:, 1].min() - 1, data[:, 1].max() + 1

    x, y = numpy.meshgrid(numpy.arange(x_min, x_max, resolution), numpy.arange(y_min, y_max, resolution))
    Z = classifier.predict(numpy.array([x.ravel(), y.ravel()]).T)
    Z = Z.reshape(x.shape)
    
    plt.pcolormesh(x, y, Z, cmap=cmap)
    plt.xlim(x.min(), x.max())
    plt.ylim(y.min(), y.max())

    colors = ('yellow', 'white', 'black')
    #cmap = ListedColormap(colors)
    #plot the data
    classes = ["setosa", "versicolor", "verginica"]
    for index, cl in enumerate(numpy.unique(labels)):
        plt.scatter(data[labels == cl, 0], data[labels == cl, 1], c=cmap(index), marker=markers[index], edgecolor="black", alpha=1.0, s=50, label=classes[index])  
 
X_combined_std = numpy.vstack((X_train_std, X_test_std))
y_combined = numpy.hstack((y_train, y_test))
plot_decision_regions(X_combined_std, y_combined, classifier=mlp)

if (standardised == False):
  xString = M[x] + " [not standardized]"  
  yString = M[y] + " [not standardized]" 
else:
  xString = M[x] + " [standardized]"  
  yString = M[y] + " [standardized]"  

plt.xlabel(xString)
plt.ylabel(yString)
plt.legend(loc='upper left')
plt.show()