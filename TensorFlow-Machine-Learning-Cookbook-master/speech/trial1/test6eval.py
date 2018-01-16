import tensorflow as tf, re
import zipfile, pandas as pd, random
import pandas as pd, scipy.io.wavfile
import numpy as np, io, os

random.seed(0)
np.random.seed(0)

labels = ['down','go','left','no','off','on','right','stop','up','yes'] + ['unknown','silence']

mfile = "/home/burak/Downloads/speech.ckpt"


num_units = 200
num_layers = 5
batch_size = 100
num_epochs = 10000
sample_rate=16000

sample_rate = 16000.0

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
    
with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())

    saver = tf.train.Saver()

    saver.restore(sess, mfile)

    zip = '/home/burak/Downloads/goog_voice_test.zip'
    import zipfile, pandas as pd, random
    import scipy.io.wavfile, io
    fout = open("/tmp/test5out.txt","w")
    fout.write("fname,label\n")
    with zipfile.ZipFile(zip, 'r') as z:
         files = z.namelist()
         for f in files:
              if '.wav' not in f: continue
              wav = io.BytesIO(z.open(f).read())
              v = scipy.io.wavfile.read(wav)
              v = v[1].reshape((1,16000))
              res = sess.run(logits, feed_dict={ pcm:v })
              fout.write("%s,%s\n" % (f.replace("test/audio/",""), labels[np.argmax(res)]) )
              fout.flush()
     #fout.close()
