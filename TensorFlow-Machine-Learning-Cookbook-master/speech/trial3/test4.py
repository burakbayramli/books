import tensorflow as tf, re
import zipfile, pandas as pd, random
import pandas as pd, scipy.io.wavfile
import numpy as np, io, os

labels = ['down','go','left','no','off','on','right','stop','up','yes']

zip = '/home/burak/Downloads/goog_voice_train.zip'
import zipfile, pandas as pd, random
import scipy.io.wavfile, io
with zipfile.ZipFile(zip, 'r') as z:
     files = z.namelist()

# add noise files many times    
noise_files = [x for x in files if '_background' in x and '.wav' in x]
for i in range(6000): files += noise_files 

files = np.array([x for x in files if  '.wav' in x] )

random.seed(0)
np.random.seed(0)

rnd_idx = np.random.choice(range(len(files)), len(files), replace=False)

training_files = files[rnd_idx[0:60000]]
val_files = files[rnd_idx[60000:-1]]

random.seed()
np.random.seed()

sample_rate = 16000
     
def get_minibatch(batch_size, training=True):
    files = training_files
    if training == False: files = val_files
    res = np.zeros((batch_size, 16000))
    y = np.zeros((batch_size,len(labels)+2 ))
    with zipfile.ZipFile(zip, 'r') as z:
        for i in range(batch_size):
          f = random.choice(files)          
          if '_background' not in f: # non-silence voice file
               label = re.findall(".*/(.*?)/.*?.wav",f)[0]
               if label in labels:
                    y[i, labels.index(label)] = 1.0
               else:
                    y[i, len(labels)] = 1.0 # unknown
               wav = io.BytesIO(z.open(f).read())
               v = scipy.io.wavfile.read(wav)
               #print f, v[1].shape
               res[i, 0:len(v[1])] = v[1]
          else: # silence, use generated data
               #print 'noise', f
               wav = io.BytesIO(z.open(f).read())
               v = scipy.io.wavfile.read(wav)
               chunks = int(len(v[1]) / sample_rate) - 1
               chosen_chunk = random.choice(range(chunks))
               fr = int(chosen_chunk * sample_rate)
               to = int((chosen_chunk+1)*sample_rate)
               chunk_byte = v[1][fr:to]
	       res[i, :] = chunk_byte
	       y[i, len(labels)+1] = 1.0 # silence
                                  
    res = res.reshape((batch_size, 16000, 1))
    return res,y
    
num_units = 300
num_layers = 1
batch_size = 100
num_epochs = 3000
mfile = "/tmp/speech.ckpt"

tf.reset_default_graph()

pcm = tf.placeholder(tf.float32, [None, 16000, 1], name = 'inputs')

y = tf.placeholder(tf.float32, shape=[None, 12])

# (batch, 128, 9) --> (batch, 64, 18)
conv1 = tf.layers.conv1d(inputs=pcm, filters=18, kernel_size=2, strides=1, 
                         padding='same', activation = tf.nn.relu)
conv1 = tf.layers.batch_normalization(conv1)
max_pool_1 = tf.layers.max_pooling1d(inputs=conv1, pool_size=2, strides=2, padding='same')

# (batch, 64, 18) --> (batch, 32, 36)
conv2 = tf.layers.conv1d(inputs=max_pool_1, filters=36, kernel_size=2, strides=1, 
                         padding='same', activation = tf.nn.relu)
conv2 = tf.layers.batch_normalization(conv2)
max_pool_2 = tf.layers.max_pooling1d(inputs=conv2, pool_size=2, strides=2, padding='same')

# (batch, 32, 36) --> (batch, 16, 72)
conv3 = tf.layers.conv1d(inputs=max_pool_2, filters=72, kernel_size=2, strides=1, 
                         padding='same', activation = tf.nn.relu)
conv3 = tf.layers.batch_normalization(conv3)
max_pool_3 = tf.layers.max_pooling1d(inputs=conv3, pool_size=2, strides=2, padding='same')

# (batch, 16, 72) --> (batch, 8, 144)
conv4 = tf.layers.conv1d(inputs=max_pool_3, filters=144, kernel_size=2, strides=1, 
                         padding='same', activation = tf.nn.relu)
conv4 = tf.layers.batch_normalization(conv4)
max_pool_4 = tf.layers.max_pooling1d(inputs=conv4, pool_size=2, strides=2, padding='same')

cells = []
for _ in range(num_layers):
    cell = tf.contrib.rnn.LSTMCell(num_units) 
    cells.append(cell)
cell = tf.contrib.rnn.MultiRNNCell(cells)
output, states = tf.nn.dynamic_rnn(cell, max_pool_4, dtype=tf.float32)
last = states[-1][0]

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

    saver = tf.train.Saver()

    if os.path.isfile(mfile + ".index"):
         print 'restoring'
         saver.restore(sess, mfile)
            
    for i in range(num_epochs):
        x_batch, y_batch = get_minibatch(batch_size)
        sess.run(train_step,feed_dict={pcm:x_batch, y:y_batch})
        if i % 5 == 0: 
            acc = sess.run(accuracy,feed_dict={pcm:x_batch, y:y_batch})
            print i, 'accuracy', acc
            saver.save(sess, mfile)
        if i % 30 == 0: 
            x_batch, y_batch = get_minibatch(batch_size, training=False)
            acc = sess.run(accuracy,feed_dict={pcm:x_batch, y:y_batch})
            print i, 'validation accuracy', acc
