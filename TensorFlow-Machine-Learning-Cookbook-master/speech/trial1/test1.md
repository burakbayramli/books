

```python
#zip = '/media/burak/New Volume/archive/data/google_voice/test.zip'
zip = '/home/burak/Downloads/goog_voice_train.zip'
import zipfile, pandas as pd, random
with zipfile.ZipFile(zip, 'r') as z:
     res = z.namelist()
print random.choice(res)
```

```text
train/audio/off/09bcdc9d_nohash_1.wav
```


```python
import tensorflow as tf
from tensorflow.contrib.signal.python.ops import mfcc_ops
from tensorflow.python.ops import array_ops
from tensorflow.python.ops import random_ops

signal = random_ops.random_normal((2, 3, 5))
with tf.Session() as sess:
     res = mfcc_ops.mfccs_from_log_mel_spectrograms(signal).eval()
     print res.shape
```

```text
(2, 3, 5)
```

```python
import tensorflow as tf

init_op = tf.global_variables_initializer()

sample_rate = 16000.0

pcm = tf.placeholder(tf.float32, [None, None])

stfts = tf.contrib.signal.stft(pcm, frame_length=1024, frame_step=256,
                               fft_length=1024)
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
```


```python
import scipy.io.wavfile, zipfile
import io, time
zip = '/home/burak/Downloads/goog_voice_train.zip'
with zipfile.ZipFile(zip, 'r') as z:
     wav = io.BytesIO(z.open('train/audio/tree/15f04ff8_nohash_0.wav').read())
     v = scipy.io.wavfile.read(wav)     
```

```python
print v[1].shape
```

```text
(16000,)
```

```python
data = v[1].reshape((1,16000))
chunk = 2000
with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     #res = sess.run(mfccs,feed_dict={pcm: data) })
     res = sess.run(mfccs,feed_dict={pcm: data[0,0:chunk].reshape(1,chunk) })
     print res.shape
     res = sess.run(mfccs,feed_dict={pcm: data[0,chunk:2*chunk].reshape(1,chunk) })
     print res.shape

```

```text
(1, 4, 80)
(1, 4, 80)
```


























