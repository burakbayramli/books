conv 1 (?, 128, 840, 16)
pool 1 (?, 64, 420, 16)
conv 2 (?, 64, 420, 32)
pool 2 (?, 32, 210, 32)
conv 3 (?, 32, 210, 32)
pool 3 (?, 16, 105, 32)
conv 4 (?, 16, 105, 32)
pool 4 (?, 8, 53, 32)

```python
import tensorflow as tf

tf.reset_default_graph()
init_op = tf.global_variables_initializer()

data = tf.placeholder(tf.float32, [1, 5, 3, 2])
X = tf.transpose(data, [0,2,1,3])
X = tf.reshape(X, (-1, 3, 10))
basic_cell = tf.contrib.rnn.GRUCell(num_units=30)
outputs, states = tf.nn.dynamic_rnn(basic_cell, X, dtype=tf.float32)
print 'gru', outputs.shape
print 'gru', states.shape

s = np.random.rand(1,5,3,2)
with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())     
     res = sess.run(states, feed_dict={data: s })
     print s[0,:,0,:]
     print sess.run(X, feed_dict={data: s })[0,0]
print res.shape

```

```text
gru (1, 3, 30)
gru (1, 30)
[[ 0.28997342  0.11505928]
 [ 0.07538374  0.19706126]
 [ 0.9662463   0.5150687 ]
 [ 0.96518386  0.41174758]
 [ 0.72776183  0.95941608]]
[ 0.28997341  0.11505928  0.07538374  0.19706126  0.96624631  0.51506871
  0.96518385  0.41174757  0.72776181  0.95941609]
(1, 30)
```









```python
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf

tf.reset_default_graph()

def weight_variable(shape):
  initial = tf.truncated_normal(shape, stddev=0.1)
  return tf.Variable(initial)

def bias_variable(shape):
  initial = tf.constant(0.1, shape=shape)
  return tf.Variable(initial)

def conv2d(x, W):
  return tf.nn.conv2d(x, W, strides=[1, 1, 1, 1], padding='SAME')

def max_pool_3x3(x):
  return tf.nn.max_pool(x, ksize=[1, 3, 3, 1],
                        strides=[1, 2, 2, 1], padding='SAME')


init_op = tf.global_variables_initializer()

data = tf.placeholder(tf.float32, [None, None])
y = tf.placeholder(tf.float32, shape=[None, 12])

stfts = tf.contrib.signal.stft(data,
			       frame_length=110,
			       frame_step=125,
			       fft_length=1678)
spectrograms = tf.abs(stfts)
spectrograms2 = tf.reshape(spectrograms,(-1,128,840,1))

W_conv1 = weight_variable([7, 7, 1, 16])
b_conv1 = bias_variable([16])
h_conv1 = tf.nn.relu(conv2d(spectrograms2, W_conv1) + b_conv1)
print 'conv 1', h_conv1.shape
h_pool1 = max_pool_3x3(h_conv1)
print 'pool 1', h_pool1.shape

W_conv2 = weight_variable([5, 5, 16, 32])
b_conv2 = bias_variable([32])
h_conv2 = tf.nn.relu(conv2d(h_pool1, W_conv2) + b_conv2)
print 'conv 2', h_conv2.shape
h_pool2 = max_pool_3x3(h_conv2)
print 'pool 2', h_pool2.shape

W_conv3 = weight_variable([3, 3, 32, 32])
b_conv3 = bias_variable([32])
h_conv3 = tf.nn.relu(conv2d(h_pool2, W_conv3) + b_conv3)
print 'conv 3', h_conv3.shape
h_pool3 = max_pool_3x3(h_conv3)
print 'pool 3', h_pool3.shape

W_conv4 = weight_variable([3, 3, 32, 32])
b_conv4 = bias_variable([32])
h_conv4 = tf.nn.relu(conv2d(h_pool3, W_conv4) + b_conv4)
print 'conv 4', h_conv4.shape
h_pool4 = max_pool_3x3(h_conv4)
print 'pool 4', h_pool4.shape

X = tf.reshape(h_pool4, (-1, 53, 32*8))

basic_cell = tf.contrib.rnn.GRUCell(num_units=30)

outputs, states = tf.nn.dynamic_rnn(basic_cell, X, dtype=tf.float32)
print 'gru', states.shape

logits = tf.contrib.layers.fully_connected(inputs=states,num_outputs=12,activation_fn=None)

softmax = tf.nn.softmax_cross_entropy_with_logits(logits=logits,labels=y) 

cross_entropy = tf.reduce_mean(softmax)

train_step = tf.train.AdamOptimizer(0.001).minimize(cross_entropy)

correct_prediction = tf.equal(tf.argmax(y,1), tf.argmax(logits,1))

accuracy = (tf.reduce_mean(tf.cast(correct_prediction, tf.float32)))*100.


from tensorflow.python.ops import random_ops
s = np.random.rand(2,16000)
with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     res = sess.run(logits, feed_dict={data: s })  
print res.shape
```

```text
conv 1 (?, 128, 840, 16)
pool 1 (?, 64, 420, 16)
conv 2 (?, 64, 420, 32)
pool 2 (?, 32, 210, 32)
conv 3 (?, 32, 210, 32)
pool 3 (?, 16, 105, 32)
conv 4 (?, 16, 105, 32)
pool 4 (?, 8, 53, 32)
gru (?, 30)
(2, 12)
```


















