
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
[   0    0    0    0    0    0    0    0    0    0  132    0    0    0    0
    0 1593    0    0    0]
[   0    0    0    0   57    0    0    0  212    0    6    0    0    0    0
 8756    1  323    0    0]
[ 132  224  116    0 1940    0    0    0    1 4552    0    0  227    0    0
  477  630    1    0    0]
[  48   21 9360    3    5  262 2047 1896   31   21   38 5062   23  194  337
  160   42    2  399  231]
[   2   31 7107   31  895    1    2   69  763    9    4  644   44  124  371
 4395   77  581   19  260]
[  17   13   27  921  272  481   79   28   38   51  240   10  118 1714   29
  342    2    2    9   62]
[ 913    2 1131   89    2   49   35    2   92   22    4    0  123   18  373
  165    3    3    3    2]
[  85   37  581  118   37    3   56   37   59 1018  309    6    2   15    1
    6   62   18   87  327]
[  40    3  140   73  108  174    3  152   72  754   24   17 1009    3   57
  206    5  102    2   26]
[ 898   13    3   85    3 1709  463    3   66  125  383  142   85   39  884
  317   39  205   83   29]
[ 19  23  13  99  13  28  23  13 216  41 179  74   4  23   5   4  41 182
   4   5]
decoder------------------------
[1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
[  58   20 2228    1  349 4478   27  173   57   27  714   62    1   20    2
    1   27  211    1    2]
[9224  173    6   29    6   92  592    6    3  116    1  224    0  232  314
 1025   14   51   88   30]
[  14    6   96   77 1292  114    6   46  203 4579   74    5   89   10   24
    0    2   54   44   34]
[  26   23    0  900  264   28  342   77   12  528  310   19   63  163 4511
   35   44  275   13   32]
[  89  519   77    8  170 1020   16 8524   23   13  470   25   26   98  177
  175  331    7  214   63]
[   8  140 1817   48   22   24    1   70   38 2214   18    0  259   48   30
 4480  321    6  507  306]
[  22  851    0    0    2  277 4655    0  215 5826  734 2949    8   84  584
   93  876    8  133    8]
[  52    0    0    0 3628    0   85    0 1503    0    1    0 5143 2686   31
 1688   81   12    0  568]
[130   0   0   0 574   0   0   0  33   0  26   0  20   0   8   0 274 816
   0   0]
[ 885    0    0    0    0    0    0    0    0    0 3087    0  248    0  251
  672 3368 1770    0    0]
[  0   0   0   0   0   0   0   0   0   0  15   0   3   0   0 133  12 113
   0   0]
[   0    0    0    0    0    0    0    0    0    0  144    0    0    0    0
    0 1896    0    0    0]
[0 0 0 0 0 0 0 0 0 0 8 0 0 0 0 0 8 0 0 0]
[   0    0    0    0    0    0    0    0    0    0 1971    0    0    0    0
    0  136    0    0    0]
[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
```







