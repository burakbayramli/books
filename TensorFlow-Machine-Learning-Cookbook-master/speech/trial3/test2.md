

```python
import tensorflow as tf

tf.reset_default_graph()

num_units = 120
num_layers = 4
batch_size = 50
num_epochs = 2000
mfile = "/tmp/speech.ckpt"

pcm = tf.placeholder(tf.float32, [None, 16000, 1], name = 'inputs')
y = tf.placeholder(tf.float32, shape=[None, 12])
conv1 = tf.layers.conv1d(inputs=pcm, filters=30, kernel_size=8, strides=1, 
                         padding='same', activation = tf.nn.relu)
print conv1.shape
max_pool_1 = tf.layers.max_pooling1d(inputs=conv1, pool_size=3, strides=4, padding='same')
print max_pool_1.shape

cells = []
for _ in range(num_layers):
    cell = tf.contrib.rnn.LSTMCell(num_units) 
    cells.append(cell)
cell = tf.contrib.rnn.MultiRNNCell(cells)
output, states = tf.nn.dynamic_rnn(cell, max_pool_1, dtype=tf.float32)
last = states[-1][0]
print last
logits = tf.contrib.layers.fully_connected(inputs=last,
                                           num_outputs=12,
                                           activation_fn=None)

softmax = tf.nn.softmax_cross_entropy_with_logits(logits=logits,labels=y) 

cross_entropy = tf.reduce_mean(softmax)
train_step = tf.train.AdamOptimizer(0.001).minimize(cross_entropy)

correct_prediction = tf.equal(tf.argmax(y,1), tf.argmax(logits,1))
accuracy = (tf.reduce_mean(tf.cast(correct_prediction, tf.float32)))*100.


with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())     
     signal = np.random.rand(1,16000,1)
     res = sess.run(max_pool_1, feed_dict={pcm: signal })
     res = sess.run(max_pool_1, feed_dict={pcm: signal })
     print 'res', res.shape

```

```text
(?, 16000, 30)
(?, 4000, 30)
Tensor("rnn/while/Exit_8:0", shape=(?, 120), dtype=float32)
res (1, 4000, 30)
```














