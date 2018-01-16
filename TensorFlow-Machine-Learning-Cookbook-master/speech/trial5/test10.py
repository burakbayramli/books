# best so far - >80 on validation
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf, re
import zipfile, pandas as pd, random
import pandas as pd, scipy.io.wavfile
import numpy as np, io, os, numpy.linalg as lin
from python_speech_features import mfcc

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
fs=16000
numcep = 26
numcontext = 9
mfile = "/tmp/speech10.ckpt"
time_dim = 50
feature_dim = 494
num_cell = 256

def normalize(v):
    if np.std(v)==0: return v
    return (v-np.mean(v)) / np.std(v)

def audiofile_to_input_vector(audio, fs, numcep, numcontext):

    # Get mfcc coefficients
    orig_inputs = mfcc(audio, samplerate=fs, numcep=numcep)

    orig_inputs = orig_inputs[::2]
    train_inputs = np.array([], np.float32)
    train_inputs.resize((orig_inputs.shape[0], numcep + 2 * numcep * numcontext))
    empty_mfcc = np.array([])
    empty_mfcc.resize((numcep))
    time_slices = range(train_inputs.shape[0])
    context_past_min = time_slices[0] + numcontext
    context_future_max = time_slices[-1] - numcontext
    for time_slice in time_slices:
        need_empty_past = max(0, (context_past_min - time_slice))
        empty_source_past = list(empty_mfcc for empty_slots in range(need_empty_past))
        data_source_past = orig_inputs[max(0, time_slice - numcontext):time_slice]
        assert(len(empty_source_past) + len(data_source_past) == numcontext)

        # Pick up to numcontext time slices in the future, and complete with empty
        # mfcc features
        need_empty_future = max(0, (time_slice - context_future_max))
        empty_source_future = list(empty_mfcc for empty_slots in range(need_empty_future))
        data_source_future = orig_inputs[time_slice + 1:time_slice + numcontext + 1]
        assert(len(empty_source_future) + len(data_source_future) == numcontext)

        if need_empty_past:
            past = np.concatenate((empty_source_past, data_source_past))
        else:
            past = data_source_past

        if need_empty_future:
            future = np.concatenate((data_source_future, empty_source_future))
        else:
            future = data_source_future

        past = np.reshape(past, numcontext * numcep)
        now = orig_inputs[time_slice]
        future = np.reshape(future, numcontext * numcep)

        train_inputs[time_slice] = np.concatenate((past, now, future))
        assert(len(train_inputs[time_slice]) == numcep + 2 * numcep * numcontext)
        
    train_inputs = (train_inputs - np.mean(train_inputs)) / np.std(train_inputs)
    res = np.zeros((time_dim, feature_dim))
    res[0:train_inputs.shape[0], 0:train_inputs.shape[1]] = train_inputs
    return res

def get_minibatch_val(batch_size):
    res = np.zeros((batch_size, time_dim, feature_dim))
    y = np.zeros((batch_size,len(labels)+2 ))
    for i in range(batch_size):
        f = random.choice(vfiles)
        label = re.findall(".*/(.*?)/.*?.wav",f)[0]
        labels2 = labels + ['unknown','silence']
        wav = io.BytesIO(zv.open(f).read())
        v = scipy.io.wavfile.read(wav)
        data = normalize(v[1])
        res[i, :] = audiofile_to_input_vector(data, fs, numcep, numcontext)
        y[i, labels2.index(label)] = 1.0
               
    return res.reshape((batch_size,time_dim,feature_dim,1)),y


def get_minibatch(batch_size):

    def noise_snippet():
       nf = random.choice(noise_files)
       wav = io.BytesIO(zt.open(nf).read())
       v = scipy.io.wavfile.read(wav)
       chunks = int(len(v[1]) / sample_rate) - 1
       chosen_chunk = random.choice(range(chunks))
       fr = int(chosen_chunk * sample_rate)
       to = int((chosen_chunk+1)*sample_rate)
       chunk_byte = v[1][fr:to]
       return normalize(chunk_byte)
       
    res = np.zeros((batch_size, time_dim, feature_dim))
    y = np.zeros((batch_size,len(labels)+2 ))
    for i in range(batch_size):
      f = random.choice(tfiles)
      # pick silence (noise) randomly as training
      if random.choice(range(10)) == 0: 
           res[i, :] = audiofile_to_input_vector(noise_snippet(), fs, numcep, numcontext)
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
              data[0:len(data)] = data + noise_snippet()[0:len(data)]
              
          mfcc = audiofile_to_input_vector(data, fs, numcep, numcontext)
          #print mfcc.shape
          res[i, :] = mfcc
          
    return res.reshape((batch_size,time_dim,feature_dim,1)),y

tf.reset_default_graph()

dropout_prob = tf.placeholder(tf.float32)

fingerprint = tf.placeholder(tf.float32, [None, time_dim, feature_dim, 1])

y = tf.placeholder(tf.float32, shape=[None, 12])

print fingerprint

layer1 = tf.layers.conv2d(inputs=fingerprint, filters=16, kernel_size=(2,2), padding='same')
layer1 = tf.layers.max_pooling2d(inputs=layer1, pool_size=(3,3), strides=1, padding='same')
layer1 = tf.layers.dropout(inputs=layer1,rate=dropout_prob)
print layer1

layer2 = tf.layers.conv2d(inputs=layer1, filters=16, kernel_size=(2,2), padding='same')
layer2 = tf.layers.max_pooling2d(inputs=layer2, pool_size=(3,3), strides=1, padding='same')
layer2 = tf.layers.dropout(inputs=layer2, rate=dropout_prob)
print layer2

input_dense = tf.reshape(layer2, (-1, 50, 494*16))

print input_dense

fc = tf.contrib.layers.fully_connected(inputs=input_dense,
                                       num_outputs=26*16,
                                       activation_fn=tf.nn.relu)

print fc

gru_fw_cell	=	tf.contrib.rnn.GRUCell(num_cell)
gru_bw_cell	=	tf.contrib.rnn.GRUCell(num_cell)

outputs, states	=  tf.nn.bidirectional_dynamic_rnn(cell_fw=gru_fw_cell,
						   cell_bw=gru_bw_cell,
						   inputs=fc,dtype=tf.float32)
print outputs

states = tf.concat(values=states, axis=1)

print states

logits = tf.contrib.layers.fully_connected(inputs=states,
                                           num_outputs=12,
                                           activation_fn=None)

pos_weight = tf.constant([0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.045, 0.505])

softmax = tf.nn.weighted_cross_entropy_with_logits(logits=logits,
                                                   targets=y,
                                                   pos_weight=pos_weight) 

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
    if i % 5 == 0:
        acc = sess.run(accuracy,feed_dict={ fingerprint:x_batch, y:y_batch, dropout_prob: 0.2 })
        print i, 'accuracy', acc
    sess.run(train_step,feed_dict={ fingerprint:x_batch, y:y_batch })
    if i % 30 == 0: 
        saver.save(sess, mfile)
        x_batch, y_batch = get_minibatch_val(batch_size)
        acc = sess.run(accuracy,feed_dict={fingerprint:x_batch, y:y_batch, dropout_prob: 0.0})
        print i, 'validation accuracy', acc
