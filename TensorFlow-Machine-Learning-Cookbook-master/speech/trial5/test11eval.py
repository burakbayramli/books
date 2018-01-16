from python_speech_features import mfcc
import tensorflow as tf, re
import zipfile, pandas as pd, random
import pandas as pd, scipy.io.wavfile
import numpy as np, io, os
import scipy.io.wavfile, io

random.seed(0)
np.random.seed(0)

labels = ['down','go','left','no','off','on','right','stop','up','yes'] + ['unknown','silence']

mfile = "/home/burak/Downloads/speech11.ckpt"
#zip = '/home/burak/Downloads/goog_voice_test.zip'    
zip = '/home/burak/Downloads/test.zip'    

sample_rate = 16000
batch_size = 100
num_epochs = 2000
fs=16000
numcep = 26
numcontext = 9
mfile = "/home/burak/Downloads/speech11.ckpt"
time_dim = 50
feature_dim = 494
num_cell = 256
outfile = "/tmp/test11out.txt"

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

def normalize(v):
    if np.std(v)==0: return v
    return (v-np.mean(v)) / np.std(v)

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
gru_fw_cell	=	tf.contrib.rnn.DropoutWrapper(gru_fw_cell, output_keep_prob=1-dropout_prob)

gru_bw_cell	=	tf.contrib.rnn.GRUCell(num_cell)
gru_bw_cell	=	tf.contrib.rnn.DropoutWrapper(gru_bw_cell, output_keep_prob=1-dropout_prob)

outputs, states	=  tf.nn.bidirectional_dynamic_rnn(cell_fw=gru_fw_cell,
						   cell_bw=gru_bw_cell,
						   inputs=fc,dtype=tf.float32)
print outputs

states = tf.concat(values=states, axis=1)

print states

logits = tf.contrib.layers.fully_connected(inputs=states,
                                           num_outputs=12,
                                           activation_fn=None)

    
with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())

    saver = tf.train.Saver()

    saver.restore(sess, mfile)

    last = None
    if os.path.isfile(outfile):
        fin = open(outfile)
        for line in fin.readlines(): pass
        last = line.split(",")[0]
    print 'last', last
    
    fout = open(outfile,"aw")
    #fout.write("fname,label\n")    
    with zipfile.ZipFile(zip, 'r') as z:
         lenFiles = len(list(z.namelist()))
         filesIt = iter(z.namelist())
         count = 0
         if last:
             f2 = None
             while (last!=f2):
                 f = filesIt.next()
                 if ".wav" not in f: continue
                 f2 = re.findall(".*/.*?/(.*?.wav)",f)[0]
                 print f
                 count += 1
             last = None
         for i in range(count, lenFiles):
             f = filesIt.next()
             if '.wav' not in f: continue                  
             print f
             f2 = re.findall(".*/.*?/(.*?.wav)",f)[0]
             wav = io.BytesIO(z.open(f).read())
             v = scipy.io.wavfile.read(wav)
             v = v[1].reshape((1,16000))
             data = normalize(v)
             m = audiofile_to_input_vector(data, fs, numcep, numcontext)
             m = m.reshape((1, 50, 494, 1))
             res = sess.run(logits, feed_dict={ fingerprint : m, dropout_prob: 0.0 })
             fout.write("%s,%s\n" % (f2, labels[np.argmax(res)]) )
             fout.flush()
     #fout.close()
