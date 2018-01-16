from python_speech_features import mfcc
import pandas as pd, util
import numpy as np
import tensorflow as tf
import scipy.io.wavfile, zipfile
import io, time, os, random, re

def adj_volume(vec):
    vol_multiplier = np.mean(np.abs(vec)) / 500.
    if vol_multiplier == 0: return vec
    vnew = vec.astype(float) / vol_multiplier
    return vnew

w = 50
h = 494
numcep = 26
numcontext = 9
fs = 16000
batch_size = 300
num_epochs = 10000
num_cell = 256
num_layers = 3
mfile = "/tmp/speech11.ckpt"
train_dir = '/home/burak/Downloads/train/audio'
val_dir = '/home/burak/Downloads/test/audio'
labels = ['down','go','left','no','off','on','right','stop','up','yes']
all_labels = labels + ['unknown','silence']
    
all_train_files = []
for d, r, f in os.walk(train_dir):
    for filename in f:
    	all_train_files.append(os.path.join(d,filename))
	
noise_files = [x for x in all_train_files if "_background_noise_" in x and ".wav" in x]
train_files = []
unknown_files = []

for x in all_train_files:
    if ".wav" in x: 
       label = re.findall(".*/(.*?)/.*?.wav",x)[0]
       if label not in labels and x not in noise_files: unknown_files.append(x)
       elif x not in noise_files: train_files.append(x)

noise_chunks = []
for f in noise_files:
    wav = io.BytesIO(open(f).read())
    v = scipy.io.wavfile.read(wav)
    noise_chunks.append(v[1])

def audiofile_to_input_vector(audio, fs, numcep, numcontext):
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
    return train_inputs
        
def get_minibatch(batch_size, silence_percent=0.20, unknown_percent=0.20):
    res = np.zeros((batch_size, w, h))
    y = np.zeros((batch_size,len(labels)+2 ))
    for i in range(batch_size):
        if random.choice(range(int(1/silence_percent))) == 0:	   
           chunk_byte = util.get_noise(noise_chunks)
	   res[i, :] = audiofile_to_input_vector(chunk_byte, fs, numcep, numcontext)
	   y[i, all_labels.index('silence')] = 1.0 # silence
        elif random.choice(range(int(1/unknown_percent))) == 0:	   
           f = random.choice(unknown_files)
	   wav = io.BytesIO(open(f).read())
	   v = scipy.io.wavfile.read(wav)
           vv = v[1]

           vv = util.tf_random_add_noise_transform(vv, noise_chunks)
           vv = util.tf_random_time_shift_transform(vv)
           vv = util.tf_random_pad_transform(vv)
           vv = util.tf_fix_pad_transform(vv)           
           
           mfcca = audiofile_to_input_vector(adj_volume(v[1]), fs, numcep, numcontext)
	   res[i, 0:mfcca.shape[0], 0:mfcca.shape[1]] = mfcca           
	   y[i, all_labels.index('unknown')] = 1.0 # unknown
	else:
	   f = random.choice(train_files)
	   wav = io.BytesIO(open(f).read())
	   v = scipy.io.wavfile.read(wav)
           vv = v[1]

           vv = util.tf_random_add_noise_transform(vv, noise_chunks)
           vv = util.tf_random_time_shift_transform(vv)
           vv = util.tf_random_pad_transform(vv)
           vv = util.tf_fix_pad_transform(vv)           
               
           mfcca = audiofile_to_input_vector(adj_volume(vv), fs, numcep, numcontext)
	   res[i, 0:mfcca.shape[0], 0:mfcca.shape[1]] = mfcca
           label = re.findall(".*/(.*?)/.*?.wav",f)[0]
	   y[i, labels.index(label)] = 1.0

    return res, y
    

all_val_files = []
for d, r, f in os.walk(val_dir):
	for filename in f:
	    if ".wav" in filename: all_val_files.append(os.path.join(d,filename))

val_x = np.zeros((len(all_val_files), w, h))
val_y = np.zeros((len(all_val_files),len(all_labels) ))
for i in range(len(all_val_files)):
    f = all_val_files[i]
    wav = io.BytesIO(open(f).read())
    v = scipy.io.wavfile.read(wav)
    label = re.findall(".*/(.*?)/.*?.wav",f)[0]
    val_x[i, :] = audiofile_to_input_vector(adj_volume(v[1]), fs, numcep, numcontext)
    val_y[i, all_labels.index(label)] = 1.0 


tf.reset_default_graph()

dropout_prob = tf.placeholder(tf.float32)

data = tf.placeholder(tf.float32, [None, w, h])

y = tf.placeholder(tf.float32, shape=[None, 12])

j_cell_fw = tf.contrib.rnn.LSTMBlockCell(num_cell,forget_bias=1.0)
j_cell_fw = tf.contrib.rnn.DropoutWrapper(j_cell_fw, input_keep_prob=1-dropout_prob)
j_cell_bw = tf.contrib.rnn.LSTMBlockCell(num_cell,forget_bias=1.0)
j_cell_bw = tf.contrib.rnn.DropoutWrapper(j_cell_bw, input_keep_prob=1-dropout_prob)

j_cell_fw = tf.contrib.rnn.MultiRNNCell([j_cell_fw]*num_layers)
j_cell_bw = tf.contrib.rnn.MultiRNNCell([j_cell_bw]*num_layers)

(j_fw, j_bw) , _ = tf.nn.bidirectional_dynamic_rnn(
    j_cell_fw,
    j_cell_bw,
    inputs=data,
    dtype=tf.float32)

outputs  = tf.concat([j_fw, j_bw],2)

print outputs

last = outputs[:,-1]

print last

#https://github.com/imalikshake/StyleNet/blob/master/model.py

logits = tf.contrib.layers.fully_connected(inputs=last,
                                           num_outputs=12,
                                           activation_fn=None)


softmax = tf.nn.softmax_cross_entropy_with_logits(logits=logits,labels=y) 

cross_entropy = tf.reduce_mean(softmax)

train_step = tf.train.AdamOptimizer(0.001).minimize(cross_entropy)

predicted_indices = tf.argmax(logits, 1)

expected_indices = tf.argmax(y, 1)

correct_prediction = tf.equal(predicted_indices, expected_indices)

evaluation_step = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))

sess = tf.Session()

sess.run(tf.global_variables_initializer())

saver = tf.train.Saver()

if os.path.isfile(mfile + ".index"):
     print 'restoring'
     saver.restore(sess, mfile)
        
for i in range(num_epochs):
    x_batch, y_batch = get_minibatch(batch_size)
    if i % 5 == 0:
        acc, _ = sess.run([evaluation_step, train_step], feed_dict={ data: x_batch, y:y_batch, dropout_prob: 0.5})
        print i, 'accuracy', acc
    if i % 30 == 0: 
        saver.save(sess, mfile)
        acc = sess.run(evaluation_step,feed_dict={ data:val_x, y:val_y, dropout_prob: 0.0})
        print i, 'val accuracy', acc

