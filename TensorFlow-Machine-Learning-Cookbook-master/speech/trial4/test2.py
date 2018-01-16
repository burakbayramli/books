from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf, re
import zipfile, pandas as pd, random
import pandas as pd, scipy.io.wavfile
import numpy as np, io, os

labels = ['down','go','left','no','off','on','right','stop','up','yes']

import zipfile, pandas as pd, random
import scipy.io.wavfile, io

trainzip = '/home/burak/Downloads/goog_voice_train.zip'
with zipfile.ZipFile(trainzip, 'r') as z: tfiles = z.namelist()
noise_files = [x for x in tfiles if '_background' in x and '.wav' in x]
tfiles =  [x for x in tfiles if '_background' not in x]
tfiles = np.array([x for x in tfiles if  '.wav' in x] )

valzip = '/home/burak/Downloads/test.zip'
with zipfile.ZipFile(valzip, 'r') as z: vfiles = z.namelist()
vfiles = np.array([x for x in vfiles if  '.wav' in x] )

zt = zipfile.ZipFile(trainzip, 'r')
zv = zipfile.ZipFile(valzip, 'r')

sample_rate = 16000
batch_size = 20
num_epochs = 5000
mfile = "/tmp/speech.ckpt"

def get_minibatch(batch_size, validation=False):

    zf = zt
    filez = tfiles
    if validation:
       zf = zv
       filez = vfiles
    
    res = np.zeros((batch_size, 16000))
    y = np.zeros((batch_size,len(labels)+2 ))
    for i in range(batch_size):
      f = random.choice(filez)
      if random.choice(range(10)) != 0 or validation==True: 
           label = re.findall(".*/(.*?)/.*?.wav",f)[0]
           labels2 = labels + ['unknown','silence']
           if label in labels2:
                y[i, labels2.index(label)] = 1.0
           else:
                y[i, len(labels)] = 1.0 # unknown
           wav = io.BytesIO(zf.open(f).read())
           v = scipy.io.wavfile.read(wav)
           res[i, 0:len(v[1])] = v[1]          
      else: 
           nf = random.choice(noise_files)
           wav = io.BytesIO(zf.open(nf).read())
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


tf.reset_default_graph()

pcm = tf.placeholder(tf.float32, [None, 16000, 1], name = 'inputs')

y = tf.placeholder(tf.float32, shape=[None, 12])

# (batch, 128, 9) --> (batch, 64, 18)
conv1 = tf.layers.conv1d(inputs=pcm, filters=18, kernel_size=2, strides=1, 
                         padding='same', activation = tf.nn.relu)
#conv1 = tf.layers.batch_normalization(conv1)
max_pool_1 = tf.layers.max_pooling1d(inputs=conv1, pool_size=2, strides=2, padding='same')

# (batch, 64, 18) --> (batch, 32, 36)
conv2 = tf.layers.conv1d(inputs=max_pool_1, filters=36, kernel_size=2, strides=1, 
                         padding='same', activation = tf.nn.relu)
#conv2 = tf.layers.batch_normalization(conv2)
max_pool_2 = tf.layers.max_pooling1d(inputs=conv2, pool_size=2, strides=2, padding='same')

# (batch, 32, 36) --> (batch, 16, 72)
conv3 = tf.layers.conv1d(inputs=max_pool_2, filters=72, kernel_size=2, strides=1, 
                         padding='same', activation = tf.nn.relu)
#conv3 = tf.layers.batch_normalization(conv3)
max_pool_3 = tf.layers.max_pooling1d(inputs=conv3, pool_size=2, strides=2, padding='same')

# (batch, 16, 72) --> (batch, 8, 144)
conv4 = tf.layers.conv1d(inputs=max_pool_3, filters=144, kernel_size=2, strides=1, 
                         padding='same', activation = tf.nn.relu)
#conv4 = tf.layers.batch_normalization(conv4)
max_pool_4 = tf.layers.max_pooling1d(inputs=conv4, pool_size=2, strides=2, padding='same')

gru_fw_cell	=	tf.contrib.rnn.GRUCell(200)
gru_fw_cell	=	tf.contrib.rnn.DropoutWrapper(gru_fw_cell)

gru_bw_cell	=	tf.contrib.rnn.GRUCell(200)
gru_bw_cell	=	tf.contrib.rnn.DropoutWrapper(gru_bw_cell)


outputs, states	=  tf.nn.bidirectional_dynamic_rnn(cell_fw=gru_fw_cell,
						   cell_bw=gru_bw_cell,
						   inputs=max_pool_4,dtype=tf.float32)
print outputs

states = tf.concat(values=states, axis=1)

logits = tf.contrib.layers.fully_connected(inputs=states,
                                           num_outputs=12,
                                           activation_fn=None)


softmax = tf.nn.softmax_cross_entropy_with_logits(logits=logits,labels=y) 

cross_entropy = tf.reduce_mean(softmax)

train_step = tf.train.AdamOptimizer(0.001).minimize(cross_entropy)

correct_prediction = tf.equal(tf.argmax(y,1), tf.argmax(logits,1))

accuracy = (tf.reduce_mean(tf.cast(correct_prediction, tf.float32)))*100.

sess = tf.Session()

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
        x_batch, y_batch = get_minibatch(batch_size, validation=True)
        acc = sess.run(accuracy,feed_dict={pcm:x_batch, y:y_batch})
        print i, 'validation accuracy', acc
