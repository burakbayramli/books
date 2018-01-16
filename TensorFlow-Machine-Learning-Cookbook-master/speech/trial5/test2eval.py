from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf, re
import zipfile, pandas as pd, random
import pandas as pd, scipy.io.wavfile
import numpy as np, io, os

random.seed(0)
np.random.seed(0)

labels = ['down','go','left','no','off','on','right','stop','up','yes'] + ['unknown','silence']

mfile = "/home/burak/Downloads/speech2.ckpt"

tf.reset_default_graph()

sample_rate = 16000
batch_size = 100
num_epochs = 2000
num_cell = 220

def normalize(v):
    norm=np.linalg.norm(v, ord=1)
    if norm==0: return v
    return v/norm


tf.reset_default_graph()

pcm = tf.placeholder(tf.float32, [None, 16000], name = 'inputs')

y = tf.placeholder(tf.float32, shape=[None, 12])

stfts = tf.contrib.signal.stft(pcm, frame_length=400, frame_step=50, fft_length=512)

spec = tf.abs(stfts)

print spec

mfcc = contrib_audio.mfcc(spec,16000,dct_coefficient_count=26)

print mfcc

gru_fw_cell	=	tf.contrib.rnn.GRUCell(num_cell)
gru_fw_cell	=	tf.contrib.rnn.DropoutWrapper(gru_fw_cell)

gru_bw_cell	=	tf.contrib.rnn.GRUCell(num_cell)
gru_bw_cell	=	tf.contrib.rnn.DropoutWrapper(gru_bw_cell)


outputs, states	=  tf.nn.bidirectional_dynamic_rnn(cell_fw=gru_fw_cell,
						   cell_bw=gru_bw_cell,
						   inputs=mfcc,dtype=tf.float32)
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

    zip = '/home/burak/Downloads/goog_voice_test.zip'
    import zipfile, pandas as pd, random
    import scipy.io.wavfile, io
    fout = open("/tmp/test2out.txt","w")
    fout.write("fname,label\n")
    with zipfile.ZipFile(zip, 'r') as z:
         files = z.namelist()
         for f in files:
              if '.wav' not in f: continue
              wav = io.BytesIO(z.open(f).read())
              v = scipy.io.wavfile.read(wav)
              v = v[1].reshape((1,16000))
              data = normalize(v)
              res = sess.run(logits, feed_dict={ pcm : data })
              fout.write("%s,%s\n" % (f.replace("test/audio/",""), labels[np.argmax(res)]) )
              fout.flush()
     #fout.close()
