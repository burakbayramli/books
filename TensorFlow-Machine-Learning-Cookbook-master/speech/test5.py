import pandas as pd
import numpy as np
import tensorflow as tf
import scipy.io.wavfile, zipfile
import io, time, os, random, re

def adj_volume(vec):
    vol_multiplier = np.mean(np.abs(vec)) / 500.
    if vol_multiplier == 0: return vec
    vnew = vec.astype(float) / vol_multiplier
    return vnew

fs = 16000
batch_size = 100
num_epochs = 10000
num_cell = 256
num_layers = 4
mfile = "/tmp/speech5.ckpt"
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
    chunks = int(len(v[1]) / fs) - 1
    for i in range(chunks):
    	fr = int(i * fs)
    	to = int((i+1)*fs)
    	chunk_byte = v[1][fr:to]
	noise_chunks.append(adj_volume(chunk_byte))
    
def get_minibatch(batch_size, silence_percent=0.10, unknown_percent=0.10, silence_added_percent = 0.2):
    res = np.zeros((batch_size, fs))
    y = np.zeros((batch_size,len(labels)+2 ))
    for i in range(batch_size):
        if random.choice(range(int(1/silence_percent))) == 0:	   
           chunk_byte = random.choice(noise_chunks)
	   res[i, :] = chunk_byte
	   y[i, all_labels.index('silence')] = 1.0 # silence
        elif random.choice(range(int(1/unknown_percent))) == 0:	   
           f = random.choice(unknown_files)
	   wav = io.BytesIO(open(f).read())
	   v = scipy.io.wavfile.read(wav)
	   res[i, 0:len(v[1])] = adj_volume(v[1])
	   y[i, all_labels.index('unknown')] = 1.0 # unknown
	else:
	   f = random.choice(train_files)
	   wav = io.BytesIO(open(f).read())
	   v = scipy.io.wavfile.read(wav)
	   res[i, 0:len(v[1])] = adj_volume(v[1])
           label = re.findall(".*/(.*?)/.*?.wav",f)[0]
	   y[i, labels.index(label)] = 1.0

    silence_added_more = int(batch_size * silence_added_percent)
    res2 = np.zeros((silence_added_more, fs))
    y2 = np.zeros((silence_added_more, len(labels)+2 ))
    for i in range(silence_added_more):
    	idx = random.choice(range(batch_size))
	res2[i, :] = adj_volume(res[idx, :] + random.choice(noise_chunks))
	y2[i] = y[idx]

    res3 = np.vstack((res,res2))
    y3 = np.vstack((y,y2))
    return res3, y3
    

all_val_files = []
for d, r, f in os.walk(val_dir):
	for filename in f:
	    if ".wav" in filename: all_val_files.append(os.path.join(d,filename))

val_x = np.zeros((len(all_val_files), fs))
val_y = np.zeros((len(all_val_files),len(all_labels) ))
for i in range(len(all_val_files)):
    f = all_val_files[i]
    wav = io.BytesIO(open(f).read())
    v = scipy.io.wavfile.read(wav)
    label = re.findall(".*/(.*?)/.*?.wav",f)[0]
    val_x[i, 0:len(v[1])] = adj_volume(v[1])
    val_y[i, all_labels.index(label)] = 1.0 


tf.reset_default_graph()

dropout_prob = tf.placeholder(tf.float32)

data = tf.placeholder(tf.float32, [None, fs])

y = tf.placeholder(tf.float32, shape=[None, 12])

stfts = tf.contrib.signal.stft(data, frame_length=256, frame_step=64, fft_length=256)

fingerprint = tf.abs(stfts)

gru_fw_cell	=	tf.contrib.rnn.GRUCell(num_cell)
gru_fw_cell	=	tf.contrib.rnn.DropoutWrapper(gru_fw_cell, output_keep_prob=1-dropout_prob)

gru_bw_cell	=	tf.contrib.rnn.GRUCell(num_cell)
gru_bw_cell	=	tf.contrib.rnn.DropoutWrapper(gru_bw_cell, output_keep_prob=1-dropout_prob)


outputs, states	=  tf.nn.bidirectional_dynamic_rnn(cell_fw=gru_fw_cell,
						   cell_bw=gru_bw_cell,
						   inputs=fingerprint,
                                                   dtype=tf.float32)
print outputs

states = tf.concat(values=states, axis=1)

print states

logits = tf.contrib.layers.fully_connected(inputs=states,
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
        acc, _ = sess.run([evaluation_step, train_step],feed_dict={ data:x_batch, y:y_batch, dropout_prob: 0.2})
        print i, 'accuracy', acc
    if i % 30 == 0: 
        saver.save(sess, mfile)
        acc = sess.run(evaluation_step,feed_dict={ data:val_x, y:val_y, dropout_prob: 0.0})
        print i, 'val accuracy', acc

