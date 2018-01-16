
```python
d = '/tmp/bla/bla'
ds = ''
for x in d.split('/'):
    if x=='': continue
    ds += '/' + x
    if os.path.isdir(ds) == False: os.mkdir(ds)
```

```text
/tmp
/tmp/bla
/tmp/bla/bla
```










```python
import shutil, os

#print help(shutil.rmtree)
print help(os.mkdir)
#os.mkdir("/tmp/asdf/")
```

```text
Help on built-in function mkdir in module posix:

mkdir(...)
    mkdir(path [, mode=0777])
    
    Create a directory.

None
```


```python
zip = '/home/burak/Downloads/goog_voice_train.zip'
import zipfile
with zipfile.ZipFile(zip, 'r') as z:
     res = z.namelist()
for x in res:
    if 'background' in x: print x
```

```text
train/audio/_background_noise_/
train/audio/_background_noise_/README.md
train/audio/_background_noise_/doing_the_dishes.wav
train/audio/_background_noise_/dude_miaowing.wav
train/audio/_background_noise_/exercise_bike.wav
train/audio/_background_noise_/pink_noise.wav
train/audio/_background_noise_/running_tap.wav
train/audio/_background_noise_/white_noise.wav
```



```python
print 1000 / 25
print 16000 / 40
```

```text
40
400
```


```python
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf

init_op = tf.global_variables_initializer()

data = tf.placeholder(tf.float32, [None, 16000])

print data

stfts = tf.contrib.signal.stft(data, frame_length=400, frame_step=100, fft_length=512)

spec = tf.abs(stfts)

print spec

mfcc = contrib_audio.mfcc(spec,16000,dct_coefficient_count=26)

print mfcc

from tensorflow.python.ops import random_ops
s = np.random.rand(1,16000)
with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     res = sess.run(spec, feed_dict={data: s })  
print res.shape
print res
```

```text
Tensor("Placeholder_6:0", shape=(?, 16000), dtype=float32)
Tensor("Abs_6:0", shape=(?, 157, 257), dtype=float32)
Tensor("Mfcc:0", shape=(?, 157, 26), dtype=float32)
(1, 157, 257)
[[[ 101.59542847   67.48015594   13.71766376 ...,    2.22408414
      3.18813014    2.85754395]
  [ 101.30989075   67.01199341   13.1284771  ...,    4.7943511
      4.46835375    1.81546021]
  [ 100.2116394    67.50292206   16.71155548 ...,    1.66668165
      6.69529486    8.80343246]
  ..., 
  [  97.54396057   65.85830688   15.92030239 ...,    1.36712921
      1.47867227    0.53499222]
  [  98.2252655    65.23770142   13.43457508 ...,    1.88137639
      1.68903661    1.20686722]
  [  97.01074219   64.02880859   12.84155464 ...,    2.79615784
      0.80926788    1.19571304]]]
```


















