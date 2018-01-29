from sklearn.neural_network.multilayer_perceptron import MLPClassifier
from sklearn import datasets
from sklearn.metrics import accuracy_score 

# Since the book came out, the cross_validation method has been moved to
# the model_selection library from the cross_validation library
#from sklearn.cross_validation import train_test_split 
from sklearn.model_selection import train_test_split

from sklearn.preprocessing import StandardScaler

iris = datasets.load_iris() 
data = iris.data 
labels = iris.target

data_train, data_test, labels_train, labels_test = train_test_split(data, labels, test_size=0.5, random_state=1)  

scaler = StandardScaler() 
scaler.fit(data) 
data_train_std = scaler.transform(data_train) 
data_test_std = scaler.transform(data_test)  

data_train = data_train_std 
data_test = data_test_std

# We add max_iter=1000 becaue the default is max_iter=200 and 
# it is not enough for full convergence 
mlp = MLPClassifier(random_state=1, max_iter=1000)
mlp.fit(data, labels)
mlp.fit(data_train, labels_train)
pred = mlp.predict(data_test)

print()
print('Misclassified samples: %d' % (labels_test != pred).sum())
print('Accuracy: %.2f' % accuracy_score(labels_test, pred))
