# best so far - >80 on validation
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf, re
import zipfile, pandas as pd, random
import pandas as pd, scipy.io.wavfile
import numpy as np, io, os, numpy.linalg as lin

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
num_epochs = 10000
mfile = "/tmp/speech12.ckpt"

def normalize(v):
    return v

def noise_snippet():
    nf = random.choice(noise_files)
    wav = io.BytesIO(zt.open(nf).read())
    v = scipy.io.wavfile.read(wav)
    chunks = int(len(v[1]) / sample_rate) - 1
    chosen_chunk = random.choice(range(chunks))
    fr = int(chosen_chunk * sample_rate)
    to = int((chosen_chunk+1)*sample_rate)
    chunk_byte = v[1][fr:to]
    return chunk_byte

 
def get_minibatch_val(batch_size):
    res = np.zeros((batch_size, 16000))
    y = np.zeros((batch_size,len(labels)+2 ))
    for i in range(batch_size):
        f = random.choice(vfiles)
        label = re.findall(".*/(.*?)/.*?.wav",f)[0]
        labels2 = labels + ['unknown','silence']
        wav = io.BytesIO(zv.open(f).read())
        v = scipy.io.wavfile.read(wav)
        data = normalize(v[1])
        res[i, 0:len(data)] = data
        y[i, labels2.index(label)] = 1.0
               
    return res,y

def get_minibatch(batch_size):
       
    res = np.zeros((batch_size, 16000))
    y = np.zeros((batch_size,len(labels)+2 ))
    for i in range(batch_size):
      f = random.choice(tfiles)
      # pick silence (noise) randomly as training
      if random.choice(range(10)) == 0: 
           res[i, :] = normalize(noise_snippet())
           y[i, len(labels)+1] = 1.0 # silence
      else: # otherwise regular file is used
          label = re.findall(".*/(.*?)/.*?.wav",f)[0]
          # label is in the file name
          if label in labels:
              y[i, labels.index(label)] = 1.0
          else: # if not unknown
              y[i, len(labels)] = 1.0 # unknown
          wav = io.BytesIO(zt.open(f).read())
          v = scipy.io.wavfile.read(wav)
          data = normalize(v[1])

          # sometimes add noise to training
          if random.choice(range(3))==0:
              res[i, 0:len(data)] = normalize(data + normalize(noise_snippet())[0:len(data)])
          else:
              res[i, 0:len(data)] = data
                                  
    return res,y


tf.reset_default_graph()

dropout_prob = tf.placeholder(tf.float32)

y = tf.placeholder(tf.float32, shape=[None, 12])

pcm = tf.placeholder(tf.float32, [None, sample_rate])

stfts = tf.contrib.signal.stft(pcm, frame_length=600, frame_step=320, fft_length=1024)

print stfts

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

mfcc_ = tf.contrib.signal.mfccs_from_log_mel_spectrograms(log_mel_spectrograms)

print mfcc_

mfcc_ = mfcc_[:, :, :26]

print mfcc_

input_time_size = 49
input_frequency_size = 26
fingerprint_4d = tf.reshape(mfcc_, [-1, input_time_size, input_frequency_size, 1])

first_conv = tf.layers.conv2d(inputs=fingerprint_4d, filters=64, kernel_size=(20,8), activation=tf.nn.relu, padding='same')
first_dropout = tf.layers.dropout(inputs=first_conv, rate=dropout_prob)
print first_conv

second_conv = tf.layers.conv2d(inputs=first_dropout, filters=64, kernel_size=(10,4), activation=tf.nn.relu, padding='same')
second_dropout = tf.layers.dropout(inputs=second_conv, rate=dropout_prob)

print second_dropout

flattened_second_conv = tf.reshape(second_dropout, [-1, 49*26*64])
print flattened_second_conv

logits = tf.contrib.layers.fully_connected(inputs=flattened_second_conv,
                                           num_outputs=12,
                                           activation_fn=None)


pos_weight = tf.constant([0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.505])

softmax = tf.nn.weighted_cross_entropy_with_logits(logits=logits,
                                                   targets=y,
                                                   pos_weight=pos_weight) 

cross_entropy = tf.reduce_mean(softmax)

#train_step = tf.train.AdamOptimizer(0.001).minimize(cross_entropy)
train_step = tf.train.GradientDescentOptimizer(0.001).minimize(cross_entropy)

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
    if i % 5 == 0:
        acc = sess.run(accuracy, feed_dict={pcm: x_batch, y: y_batch, dropout_prob: 0.0 })
        print i, 'accuracy', acc
    sess.run(train_step, feed_dict={pcm:x_batch, y:y_batch, dropout_prob: 0.2})
    if i % 30 == 0: 
        saver.save(sess, mfile)
        x_batch, y_batch = get_minibatch_val(batch_size)
        acc = sess.run(accuracy,feed_dict={pcm: x_batch, y: y_batch, dropout_prob: 0.0})
        print i, 'validation accuracy', acc
