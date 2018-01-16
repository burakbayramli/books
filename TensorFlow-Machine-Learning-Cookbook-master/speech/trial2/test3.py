import tensorflow as tf, re
import zipfile, pandas as pd, random
import pandas as pd, scipy.io.wavfile
import numpy as np, io, os

labels = ['down','go','left','no','off','on','right','stop','up','yes']

num_units = 300
batch_size = 10
num_epochs = 10000
sample_rate = 16000
mfile = "/tmp/speech.ckpt"

zip = '/home/burak/Downloads/goog_voice_train.zip'
import zipfile, pandas as pd, random
import scipy.io.wavfile, io
with zipfile.ZipFile(zip, 'r') as z:
     files = z.namelist()

# add noise files many times    
noise_files = [x for x in files if '_background' in x and '.wav' in x]
for i in range(20): files += noise_files 

files = np.array([x for x in files if  '.wav' in x] )

random.seed(0)
np.random.seed(0)

rnd_idx = np.random.choice(range(len(files)), len(files), replace=False)

training_files = files[rnd_idx[0:60000]]
val_files = files[rnd_idx[60000:-1]]

random.seed()
np.random.seed()
     
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
                                  
    return res,y
    
sample_rate = 16000.0

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

pcm = tf.placeholder(tf.float32, [None, None])
y = tf.placeholder(tf.float32, shape=[None, 12])

stfts = tf.contrib.signal.stft(pcm,
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

X = tf.transpose(h_pool4, [0,2,1,3])
X = tf.reshape(X, (-1, 53, 32*8))

basic_cell = tf.contrib.rnn.GRUCell(num_units=num_units)

outputs, states = tf.nn.dynamic_rnn(basic_cell, X, dtype=tf.float32)
print 'gru', states.shape

logits = tf.contrib.layers.fully_connected(inputs=states,num_outputs=12,activation_fn=None)

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
