import tensorflow as tf
from python_speech_features import mfcc
import numpy as np
import tensorflow as tf
import scipy.io.wavfile as wav
from glob import glob
import time, re, os, random
import numpy as np

random.seed(0)
np.random.seed(0)
num_units = 20
num_layers = 2
batch_size = 20
num_epochs = 2000
sample_rate=16000
num_features = 26
steps_before_after = 9
mfile = "/tmp/speech.ckpt"
labels = ['down','go','left','no','off','on','right','stop','up','yes']

def load_wavfile(wavfile):

    rate, sig = wav.read(wavfile)
    data_name = os.path.splitext(os.path.basename(wavfile))[0]
    return rate, sig, data_name

def audiofile_to_input_vector(audio_filename, numcep, numcontext):

    # Load wav files
    fs, audio = wav.read(audio_filename)

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
    return train_inputs
    
def find_files(directory, pattern='.wav'):
    """Recursively finds all files matching the pattern."""
    files = []
    for root, directories, filenames in os.walk(directory):
        for filename in filenames: 
            path = os.path.join(root,filename)
            if pattern in path: files.append(path)    
    res = sorted(files)
    return res

audio_files = find_files("/home/burak/Downloads/goog_tf_speech/audio")

def get_minibatch(batch_size):
    res = np.zeros((batch_size, 50, 494))
    y = np.zeros((batch_size,len(labels)))
    i = 0
    while True:
    	f = random.choice(audio_files)
	a = audiofile_to_input_vector(f, num_features, steps_before_after)
	a = a.astype('float32')
        if a.shape[0] != 50: continue
    	res[i, :, :] = a
	label = re.findall(".*/(.*?)/.*?.wav",f)[0]
        #print label
	y[i, labels.index(label)] = 1.0
        i += 1
        if i==batch_size: break

    return res, y

if __name__ == "__main__":

    tf.reset_default_graph()

    X = tf.placeholder(tf.float32, [batch_size, 50, 494])
    y = tf.placeholder(tf.float32, shape=[batch_size, len(labels)])

    cells = []
    for _ in range(num_layers):
        cell = tf.contrib.rnn.LSTMCell(num_units) 
        cells.append(cell)
    cell = tf.contrib.rnn.MultiRNNCell(cells)
    output, states = tf.nn.dynamic_rnn(cell, X, dtype=tf.float32)
    last = states[-1][0]
    logits = tf.contrib.layers.fully_connected(inputs=last,
                                               num_outputs=len(labels),
                                               activation_fn=None)

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
            sess.run(train_step,feed_dict={X:x_batch, y:y_batch})
            if i % 20 == 0: 
                acc = sess.run(accuracy,feed_dict={X:x_batch, y:y_batch})
                print i, 'accuracy', acc
                saver.save(sess, mfile)
