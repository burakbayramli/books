

```python
#zip = '/media/burak/New Volume/archive/data/google_voice/test.zip'
zip = '/home/burak/Downloads/goog_voice_train.zip'
import zipfile, pandas as pd, random
with zipfile.ZipFile(zip, 'r') as z:
     res = z.namelist()
```


```python
f = random.choice(res)
print f	 
```

```text
test/audio/clip_5c76812f0.wav
```

```python
train_files = [x for x in res if not '_background_noise_' in x]
noise_files = [x for x in res if '_background_noise_' in x]
```

```python
print len(train_files)
print len(noise_files)
```

```text
64757
8
```

```python
import numpy as np, io, os, acoustics
import pandas as pd, scipy.io.wavfile
f = random.choice(noise_files)
with zipfile.ZipFile(zip, 'r') as z:
     wav = io.BytesIO(z.open(f).read())
     v = scipy.io.wavfile.read(wav)
     print v[1].shape
```

```text
(1522930,)
```

```python
sample_rate = 16000.
chunks = int(len(v[1]) / 16000.) - 1
chosen_chunk = random.choice(range(chunks))
fr = int(chosen_chunk * sample_rate)
to = int((chosen_chunk+1)*sample_rate)
chunk_byte = v[1][fr:to]
print len(chunk_byte)
```

```text
16000
```








