

```python
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
import tensorflow as tf

init_op = tf.global_variables_initializer()

data = tf.placeholder(tf.float32, [None, None])

stfts = tf.contrib.signal.stft(data,
			       frame_length=110,
			       frame_step=125,
			       fft_length=1678)
spectrograms = tf.abs(stfts)

from tensorflow.python.ops import random_ops
s = np.random.rand(2,16000)
with tf.Session() as sess:
     sess.run(tf.global_variables_initializer())
     res = sess.run(spectrograms, feed_dict={data: s })  
print res.shape
```

```text
(2, 128, 840)
```


















