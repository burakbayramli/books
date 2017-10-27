
```python
import os
import re
import sys
import string
import requests
import io
import numpy as np
import collections
import random
import pickle
import string
import matplotlib.pyplot as plt
import tensorflow as tf
from zipfile import ZipFile
from collections import Counter

local_repository = 'temp'

# Model Parameters
learning_rate = 0.1
lr_decay_rate = 0.99
lr_decay_every = 100
max_gradient = 5.0
batch_size = 50
num_layers = 3
rnn_size = 500
layer_size = 512
generations = 10000
vocab_size = 10000
save_every = 1000
eval_every = 500
output_every = 50
punct = string.punctuation

# Data Parameters
data_dir = 'temp'
#data_file = '/home/burak/Downloads/tur-eng/tur.txt'
data_file = '/home/burak/Downloads/deu-eng/deu.txt'
model_path = 'seq2seq_model'
full_model_dir = os.path.join(data_dir, model_path)

eng_ger_data = []
with open(os.path.join(data_dir, data_file), 'r') as in_conn:
    for row in in_conn:
        eng_ger_data.append(row[:-1])

# Remove punctuation
eng_ger_data = [''.join(char for char in sent if char not in punct) for sent in eng_ger_data]
# Split each sentence by tabs    
eng_ger_data = [x.split('\t') for x in eng_ger_data if len(x)>=1]
[english_sentence, german_sentence] = [list(x) for x in zip(*eng_ger_data)]
english_sentence = [x.lower().split() for x in english_sentence]
german_sentence = [x.lower().split() for x in german_sentence]

print('Processing the vocabularies.')
# Process the English Vocabulary
all_english_words = [word for sentence in english_sentence for word in sentence]
all_english_counts = Counter(all_english_words)
eng_word_keys = [x[0] for x in all_english_counts.most_common(vocab_size-1)] #-1 because 0=unknown is also in there
eng_vocab2ix = dict(zip(eng_word_keys, range(1,vocab_size)))
eng_ix2vocab = {val:key for key, val in eng_vocab2ix.items()}
english_processed = []
for sent in english_sentence:
    temp_sentence = []
    for word in sent:
        try:
            temp_sentence.append(eng_vocab2ix[word])
        except:
            temp_sentence.append(0)
    english_processed.append(temp_sentence)

    
# Process the German Vocabulary
all_german_words = [word for sentence in german_sentence for word in sentence]
all_german_counts = Counter(all_german_words)
ger_word_keys = [x[0] for x in all_german_counts.most_common(vocab_size-1)]
ger_vocab2ix = dict(zip(ger_word_keys, range(1,vocab_size)))
ger_ix2vocab = {val:key for key, val in ger_vocab2ix.items()}
german_processed = []
for sent in german_sentence:
    temp_sentence = []
    for word in sent:
        try:
            temp_sentence.append(ger_vocab2ix[word])
        except:
            temp_sentence.append(0)
    german_processed.append(temp_sentence)
```

```text
Processing the vocabularies.
```

```python
print ger_vocab2ix.keys()[:10]
print all_german_words[:10]
```

```text
['abgehalten', 'abfahren', 't\xc3\xbcrkisch', 'dienste', 'l\xc3\xa4cherlich', 'nieder', 'geschneit', 'posteingang', 'schlafende', 'senkte']
['hallo', 'gr\xc3\xbc\xc3\x9f', 'gott', 'lauf', 'potzdonner', 'donnerwetter', 'feuer', 'hilfe', 'zu', 'h\xc3\xbclf']
```

```python
test_english = ['hello where is my computer',
                'the quick brown fox jumped over the lazy dog',
                'is it going to rain tomorrow']
test_data = []
for sentence in test_english:
    temp_sentence = []
    for word in sentence.split(' '):
        try:
            temp_sentence.append(eng_vocab2ix[word])
        except:
            # Use '0' if the word isn't in our vocabulary
            temp_sentence.append(0)
    test_data.append(temp_sentence)
for t in test_data: print t
```

```text
[1853, 90, 7, 19, 545]
[1, 1709, 1483, 2512, 1375, 160, 1, 1560, 190]
[7, 11, 79, 2, 401, 173]
```
```python
# Define Buckets for sequence lengths
# We will split data into the corresponding buckets:
# (x1, y1), (x2, y2), ...
# Where all entries in bucket 1: len(x)<x1 and len(y)<y1 and so on.
x_maxs = [5, 7, 11, 50]
y_maxs = [10, 12, 17, 60]
buckets = [x for x in zip(x_maxs, y_maxs)]
bucketed_data = [[] for _ in range(len(x_maxs))]
for eng, ger in zip(english_processed, german_processed):
    for ix, (x_max, y_max) in enumerate(zip(x_maxs, y_maxs)):
        if (len(eng) <= x_max) and (len(ger) <= y_max):
            bucketed_data[ix].append([eng, ger])
            break

# Print summaries of buckets
train_bucket_sizes = [len(bucketed_data[b]) for b in range(len(buckets))]
train_total_size = float(sum(train_bucket_sizes))
for ix, bucket in enumerate(bucketed_data):
    print('Data pts in bucket {}: {}'.format(ix, len(bucket)))
```

```text
Data pts in bucket 0: 59300
Data pts in bucket 1: 49127
Data pts in bucket 2: 37110
Data pts in bucket 3: 7283
```

```python
import data_utils
batch_size = 20

def get_batch(data, bucket_id):
  encoder_size, decoder_size = buckets[bucket_id]
  encoder_inputs, decoder_inputs = [], []
  for _ in xrange(batch_size):
    encoder_input, decoder_input = random.choice(data[bucket_id])
    encoder_pad = [data_utils.PAD_ID] * (encoder_size - len(encoder_input))
    encoder_inputs.append(list(reversed(encoder_input + encoder_pad)))
    decoder_pad_size = decoder_size - len(decoder_input) - 1
    decoder_inputs.append([data_utils.GO_ID] + decoder_input +
                          [data_utils.PAD_ID] * decoder_pad_size)

  batch_encoder_inputs, batch_decoder_inputs, batch_weights = [], [], []

  for length_idx in xrange(encoder_size):
    batch_encoder_inputs.append(
        np.array([encoder_inputs[batch_idx][length_idx]
                  for batch_idx in xrange(batch_size)], dtype=np.int32))

  for length_idx in xrange(decoder_size):
    batch_decoder_inputs.append(
        np.array([decoder_inputs[batch_idx][length_idx]
                  for batch_idx in xrange(batch_size)], dtype=np.int32))

    batch_weight = np.ones(batch_size, dtype=np.float32)
    for batch_idx in xrange(batch_size):
      if length_idx < decoder_size - 1:
        target = decoder_inputs[batch_idx][length_idx + 1]
      if length_idx == decoder_size - 1 or target == data_utils.PAD_ID:
        batch_weight[batch_idx] = 0.0
    batch_weights.append(batch_weight)
  return batch_encoder_inputs, batch_decoder_inputs, batch_weights


rand_bucket_ix = np.random.choice(len(bucketed_data))    
model_outputs = get_batch(bucketed_data, rand_bucket_ix)
encoder_inputs, decoder_inputs, target_weights = model_outputs
print 'encoder------------------------'
for x in encoder_inputs:
    print x
print 'decoder------------------------'
for x in decoder_inputs:
    print x
```

```text
encoder------------------------
[   0    0    0    0    0    0    0  224    0    0    0 2599 3831    0    0
    0    0    0    0    0]
[ 0  0  0  0  0  0  0 19  0  0  0  2 25  0  0  0  0  0  0  0]
[   0 2510    0    0   18    0    0   10  217  132 2985  604   88  175    0
  268    0  145  105    0]
[1383 5697 1018   42  235  356   17 5848  187   12  999   24  377  689  548
   86 2747   64   48  597]
[   1 5240    1   33   14    1   82    0    1  205    6   27   94    2   43
  328  360 1109   10   48]
[  27  304   18    4    5   10    2  288   34    2   33 4918   14   17  231
    2   24    7 2002   18]
[ 424    9 1515   67   23 1766   37    1 2010  241   14   10  212   18   43
 2487   10    5   14  591]
[ 628 3852    1  117  265    1    3   10  400    5   11  119    1 1349  210
    1 1894   67  488  131]
[ 10  93  87 921  66  17 102  72   2  67  12   6   2  95  28  55   9  36
 156   6]
[ 688  428   46    7   15  142   51   14   15  521  155 1212 5392   68   93
  326  890    3  642  141]
[ 6 66  3  8  4 74  7 11  3  4  8  5  6 11 16  1  8 13  1 30]
decoder------------------------
[1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
[ 15 119   6  11   1  62 126  10   5   1  11   2  25  47   7  12  11 281
   9   5]
[2057  463  566    3   18 1508   10   29  138 1242  228 1329 1310  108  196
  322 9456    6 2091    0]
[1043  196   24  900   69   19   54   25   31  118   10   21  123    8   40
   29 1697   65  450   15]
[ 554   22 1106 1480  594    9    7   12   12   65    0  200 2465  723  256
   40  200    2  234  446]
[  21   13   63  158 2259 4248    6    0  420    2  102    0   29  107   27
 1100  466   12   73  683]
[  24    9 1742    1    2  123   19 6159 4507    7    0   80 2490    1  521
   51 3420  174 1192   39]
[ 790 8675 3634   60  978 2943   95  616 5347  144    0 7565   41 2126 1574
    0    0    0 2544  105]
[   0 2448    4    0   14    0  173 1309    0  275    0    0   17  330   70
    0    0    0 6418  538]
[   0  356    0    0    0    0    0    0    0   83    0    0 1706    0    0
    0    0    0    0    0]
[  0 320   0   0   0   0   0   0   0   0   0   0 404   0   0   0   0   0
   0   0]
[   0 2333    0    0    0    0    0    0    0    0    0    0    0    0    0
    0    0    0    0    0]
[   0 9907    0    0    0    0    0    0    0    0    0    0    0    0    0
    0    0    0    0    0]
[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
```






