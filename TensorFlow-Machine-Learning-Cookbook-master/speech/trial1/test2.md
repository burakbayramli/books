

```python
zip = '/home/burak/Downloads/goog_voice_test.zip'
import zipfile, pandas as pd, random
import scipy.io.wavfile, io
with zipfile.ZipFile(zip, 'r') as z:
     training_files = z.namelist()

print len(training_files)
```

```text
158540
```

```python
import re

labels = ['down','go','left','no','off','on','right','stop','up','yes']

def get_minibatch(batch_size):
    res = np.zeros((batch_size, 16000))
    y = np.zeros((batch_size,len(labels)+2 ))
    with zipfile.ZipFile(zip, 'r') as z:
        for i in range(batch_size):
            f = random.choice(training_files)
	    label = re.findall(".*/(.*?)/.*?.wav",f)[0]
	    if label in labels:
	       y[i, labels.index(label)] = 1.0
	    else:
	       y[i, len(labels)+1] = 1.0 # unknown
     	    wav = io.BytesIO(z.open(f).read())
     	    v = scipy.io.wavfile.read(wav)
	    res[i, 0:len(v[1])] = v[1]
    return res,y
    
X,y = get_minibatch(1)
print X.shape
print y.shape
```

```text
(1, 16000)
(1, 12)
```


















```python
def get_minibatch(batch_size):
    res = np.zeros((batch_size, 16000))
    with zipfile.ZipFile(zip, 'r') as z:
        for i in range(batch_size):
            f = random.choice(training_files)
     	    wav = io.BytesIO(z.open(f).read())
     	    v = scipy.io.wavfile.read(wav)
	    res[i, 0:len(v[1])] = v[1]
    return res
    
tmp = get_minibatch(20)
print tmp.shape
```

```text
(20, 16000)
```

```python
import scipy.io.wavfile, zipfile
import io, time
zip = '/home/burak/Downloads/goog_voice_train.zip'
with zipfile.ZipFile(zip, 'r') as z:
     wav = io.BytesIO(z.open('train/audio/off/09bcdc9d_nohash_1.wav').read())
     v = scipy.io.wavfile.read(wav)     
```

```python
import tensorflow as tf

init_op = tf.global_variables_initializer()

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

mfccs2 = tf.reshape(mfccs, (-1, 30, 80))
```


```python
data = v[1].reshape((1,16000))
with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     res = sess.run(mfccs2,feed_dict={pcm: data })
     print res.shape
```

```text
(1, 30, 80)
```

```python
with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     res = sess.run(mfccs2,feed_dict={pcm: tmp })
     print res.shape
```

```text
(20, 30, 80)
```





































