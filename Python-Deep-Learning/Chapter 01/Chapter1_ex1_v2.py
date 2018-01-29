from sklearn.neural_network.multilayer_perceptron import MLPClassifier
from sklearn import datasets
from sklearn.metrics import accuracy_score 

iris = datasets.load_iris() 
data = iris.data 
labels = iris.target

# We add max_iter=1000 becaue the default is max_iter=200 and 
# it is not enough for full convergence
mlp = MLPClassifier(random_state=1, max_iter=1000) 
mlp.fit(data, labels)

pred = mlp.predict(data)

print()
print('Accuracy: %.2f' % accuracy_score(labels, pred))
