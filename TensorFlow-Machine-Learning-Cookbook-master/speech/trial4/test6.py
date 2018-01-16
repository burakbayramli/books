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
batch_size = 100
num_epochs = 5000
mfile = "/tmp/speech6.ckpt"
num_units = 100
num_layers = 3

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
                                  
    return res,y

tf.reset_default_graph()

pcm = tf.placeholder(tf.float32, [None, None])

pcm2 = pcm[:,0:16000]

pcm3 = tf.reshape(pcm2, (-1,10,1600))

stfts = tf.contrib.signal.stft(pcm3, frame_length=1024, frame_step=256, fft_length=1024)

spectrograms = tf.abs(stfts)

num_spectrogram_bins = stfts.shape[-1].value
lower_edge_hertz, upper_edge_hertz, num_mel_bins = 80.0, 7600.0, 80
linear_to_mel_weight_matrix = tf.contrib.signal.linear_to_mel_weight_matrix(
  num_mel_bins, num_spectrogram_bins, sample_rate, lower_edge_hertz,
  upper_edge_hertz)
  
mel_spectrograms = tf.tensordot(spectrograms, linear_to_mel_weight_matrix, 1)
  
mel_spectrograms.set_shape(spectrograms.shape[:-1].concatenate(
  linear_to_mel_weight_matrix.shape[-1:]))

log_mel_spectrograms = tf.log(mel_spectrograms + 1e-6)

mfccs = tf.contrib.signal.mfccs_from_log_mel_spectrograms(log_mel_spectrograms)

X = tf.reshape(mfccs, (-1, 30, 80))
y = tf.placeholder(tf.float32, shape=[batch_size, 12])

cells = []
for _ in range(num_layers):
    cell = tf.contrib.rnn.LSTMCell(num_units) 
    cells.append(cell)
cell = tf.contrib.rnn.MultiRNNCell(cells)
output, states = tf.nn.dynamic_rnn(cell, X, dtype=tf.float32)
last = states[-1][0]

logits = tf.contrib.layers.fully_connected(inputs=last,
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
