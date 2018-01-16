
```python
zip = '/home/burak/Downloads/goog_voice_train.zip'
import zipfile, pandas as pd, random
with zipfile.ZipFile(zip, 'r') as z:
     res = z.namelist()
```

```python
print random.choice(res)
```

```text
train/audio/sheila/019fa366_nohash_0.wav
```


```python
import scipy.io.wavfile, zipfile
import io, time
zip = '/home/burak/Downloads/goog_voice_train.zip'
f = 'train/audio/sheila/019fa366_nohash_0.wav'
with zipfile.ZipFile(zip, 'r') as z:
     wav = io.BytesIO(z.open(f).read())
     v = scipy.io.wavfile.read(wav)     
```

```python
print v[1]
```

```text
[0 3 5 ..., 4 4 3]
```

```python
plt.plot(v[1])
plt.savefig('out.png')
```

```python
def normalize(x):
    print x.max()-x.min()
    print x.std()
    x2 = x / x.std()
    return x2

s = normalize(v[1])
print s
plt.plot(s)
plt.savefig('out2.png')
```

```text
14587
1525.28662483
[ 0.          0.00196684  0.00327807 ...,  0.00262246  0.00262246
  0.00196684]
```

```python
fs = 16000
#scipy.io.wavfile.write('/tmp/tmp.wav', fs, s)
scipy.io.wavfile.write('/tmp/tmp.wav', fs, v[1])
```



```python
import tensorflow as tf
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio

import tensorflow as tf

tf.reset_default_graph()

sample_rate = 16000.0

pcm = tf.placeholder(tf.float32, [16000, 2])

#res = contrib_audio.audio_spectrogram(pcm,window_size=480,stride=160,magnitude_squared=True)
res = contrib_audio.audio_spectrogram(pcm,window_size=480,stride=160,magnitude_squared=True)

with tf.Session() as sess:
     data = np.random.randn(16000, 2)
     res = sess.run(res,feed_dict={pcm: data})
     print res.shape
```

```text
(1, 98, 257)
```









```python
from python_speech_features import mfcc
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
    return train_inputs
    
fs=16000
numcep = 13
numcontext = 9

data = np.random.randn(1,16000)
res = audiofile_to_input_vector(data, fs, numcep, numcontext)
print res.shape
```

```text
(50, 247)
```









```python
a = [3,4,3,4,5]

it = iter(a)
print it.next()
print it.next()
print it.next()
print it.next()
print it.end()
```

```text
3
4
3
4
5
```


```python
import zipfile

zip = '/home/burak/Downloads/goog_voice_test.zip'

with zipfile.ZipFile(zip, 'r') as z:
     filesit = iter(z.namelist())

```

```python
print filesit.next()
print filesit.next()
print filesit.next()
print filesit.next()
print filesit.next()
print filesit.
```

```text
test/audio/
test/audio/clip_7afff8114.wav
test/audio/clip_d7129a6e9.wav
test/audio/clip_2fba10c36.wav
test/audio/clip_977e1b901.wav
```


```python
import re
s = 'test/audio/clip_977e1b901.wav'
print re.findall(".*/.*?/(.*?.wav)",s)[0]
```

```text
clip_977e1b901.wav
```






