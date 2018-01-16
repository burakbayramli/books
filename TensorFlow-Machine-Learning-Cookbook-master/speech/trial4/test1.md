
```python
import scipy.io.wavfile, zipfile
import io, numpy.linalg as lin
zip = '/home/burak/Downloads/goog_voice_train.zip'
with zipfile.ZipFile(zip, 'r') as z:
     wav = io.BytesIO(z.open('train/audio/tree/15f04ff8_nohash_0.wav').read())
     v = scipy.io.wavfile.read(wav)
     print v[1]
```

```text
[  2 -10 -21 ..., -14  19  13]
```


```python
def normalize(v):
    norm=np.linalg.norm(v, ord=1)
    if norm==0: norm=np.finfo(v.dtype).eps
    return v/norm

print normalize(v[1])
```

```text
[  4.44844112e-07  -2.22422056e-06  -4.67086318e-06 ...,  -3.11390879e-06
   4.22601907e-06   2.89148673e-06]
```







```python
import zipfile
zip = '/home/burak/Downloads/goog_voice_train.zip'
z = zipfile.ZipFile(zip, 'r')
z.close()
```

```python
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf, re
import zipfile, pandas as pd, random
import pandas as pd, scipy.io.wavfile
import numpy as np, io, os

labels = ['down','go','left','no','off','on','right','stop','up','yes']


import zipfile, pandas as pd, random
import scipy.io.wavfile, io

trainzip = '/home/burak/Downloads/goog_voice_train.zip'
with zipfile.ZipFile(trainzip, 'r') as z: tfiles = z.namelist()
noise_files = [x for x in tfiles if 'noise.wav' in x]
tfiles =  [x for x in tfiles if '_background' not in x]
tfiles = np.array([x for x in tfiles if  '.wav' in x] )

valzip = '/home/burak/Downloads/test.zip'
with zipfile.ZipFile(valzip, 'r') as z: vfiles = z.namelist()
vfiles = np.array([x for x in vfiles if  '.wav' in x] )

print tfiles[:10]
print vfiles[:10]

zt = zipfile.ZipFile(trainzip, 'r')
zv = zipfile.ZipFile(valzip, 'r')
```

```text
['train/audio/bed/00176480_nohash_0.wav'
 'train/audio/bed/004ae714_nohash_0.wav'
 'train/audio/bed/004ae714_nohash_1.wav'
 'train/audio/bed/00f0204f_nohash_0.wav'
 'train/audio/bed/00f0204f_nohash_1.wav'
 'train/audio/bed/012c8314_nohash_0.wav'
 'train/audio/bed/012c8314_nohash_1.wav'
 'train/audio/bed/0132a06d_nohash_0.wav'
 'train/audio/bed/0135f3f2_nohash_0.wav'
 'train/audio/bed/0137b3f4_nohash_0.wav']
['test/audio/down/clip_1fc8b54ac.wav' 'test/audio/down/clip_3fd4d5983.wav'
 'test/audio/down/clip_4240be2d8.wav' 'test/audio/down/clip_4fc880247.wav'
 'test/audio/down/clip_6853c3d86.wav' 'test/audio/down/clip_7b9a54b3f.wav'
 'test/audio/down/clip_7f8470799.wav' 'test/audio/down/clip_caf7bee3b.wav'
 'test/audio/down/clip_cf7b57412.wav' 'test/audio/down/clip_cfda138a6.wav']
```





```python
sample_rate = 16000

def get_minibatch(batch_size, validation=False):

    zf = zt
    filez = tfiles
    if validation:
       zf = zv
       filez = vfiles
    
    res = np.zeros((batch_size, 16000))
    y = np.zeros((batch_size,len(labels)+2 ))
    for i in range(batch_size):
      f = random.choice(filez)          
      if random.choice(range(10)) != 0:
           label = re.findall(".*/(.*?)/.*?.wav",f)[0]
           if label in labels:
                y[i, labels.index(label)] = 1.0
           else:
                y[i, len(labels)] = 1.0 # unknown
           wav = io.BytesIO(zf.open(f).read())
           v = scipy.io.wavfile.read(wav)
           res[i, 0:len(v[1])] = v[1]
      elif validation==False: 
           nf = random.choice(noise_files)
           wav = io.BytesIO(z.open(nf).read())
           v = scipy.io.wavfile.read(wav)
           chunks = int(len(v[1]) / sample_rate) - 1
           chosen_chunk = random.choice(range(chunks))
           fr = int(chosen_chunk * sample_rate)
           to = int((chosen_chunk+1)*sample_rate)
           chunk_byte = v[1][fr:to]
           res[i, :] = chunk_byte
           y[i, len(labels)+1] = 1.0 # silence
                                  
    return res,y

x,y = get_minibatch(10)
```

```python
print y
```

```text
[[ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  1.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  1.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]]
```


```python
x,y = get_minibatch(10,validation=True)
print y
```

```text
[[ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]
 [ 0.  0.  0.  1.  0.  0.  0.  0.  0.  0.  0.  0.]
 [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.]]
```







