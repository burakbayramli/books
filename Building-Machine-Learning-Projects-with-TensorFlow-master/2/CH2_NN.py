import tensorflow as tf
import numpy as np
import time

import matplotlib
import matplotlib.pyplot as plt

from sklearn.datasets.samples_generator import make_circles

N=210
K=2
# Maximum number of iterations, if the conditions are not met
MAX_ITERS = 1000
cut=int(N*0.7)

start = time.time()

data, features = make_circles(n_samples=N, shuffle=True, noise= 0.12, factor=0.4)
tr_data, tr_features= data[:cut], features[:cut]
te_data,te_features=data[cut:], features[cut:]

fig, ax = plt.subplots()
ax.scatter(tr_data.transpose()[0], tr_data.transpose()[1], marker = 'o', s = 100, c = tr_features, cmap=plt.cm.coolwarm )
plt.plot()

points=tf.Variable(data)
cluster_assignments = tf.Variable(tf.zeros([N], dtype=tf.int64))

sess = tf.Session()
sess.run(tf.initialize_all_variables())

test=[]

for i, j in zip(te_data, te_features):
    distances = tf.reduce_sum(tf.square(tf.sub(i , tr_data)),reduction_indices=1)
    neighbor = tf.arg_min(distances,0)
    
    #print tr_features[sess.run(neighbor)]
    #print j
    test.append(tr_features[sess.run(neighbor)])
print test
fig, ax = plt.subplots()
ax.scatter(te_data.transpose()[0], te_data.transpose()[1], marker = 'o', s = 100, c = test, cmap=plt.cm.coolwarm )
plt.plot()

#rep_points_v = tf.reshape(points, [1, N, 2])
#rep_points_h = tf.reshape(points, [N, 2])
#sum_squares = tf.reduce_sum(tf.square(rep_points - rep_points), reduction_indices=2)
#print(sess.run(tf.square(rep_points_v - rep_points_h)))

end = time.time()
print ("Found in %.2f seconds" % (end-start))
print "Cluster assignments:", test


