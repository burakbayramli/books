
# Creating Sequence to Sequence Models

----------------------------------

Here we show how to implement sequence to sequence models. Specifically, we will build an English to German translation model.


We start by loading the necessary libraries


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
from tensorflow.python.framework import ops
ops.reset_default_graph()
```

TensorFlow has a separate `models` folder that holds some great models that we can use.  One of these is a seq2seq model that we will use to train on our translation data. The models are located here:

https://github.com/tensorflow/models

And specifically, the seq2seq model and utilities are located here:

https://github.com/tensorflow/models/tree/master/tutorials/rnn/translate
    
If you want to download them separately, you may do that.  The goal
here is to access the `data_utils.py` and the `seq2seq_model.py`
scripts in the above folder.  We will import them.

The following block of code will import the whole `models` repository
into the temp folder.


```python
local_repository = 'temp'
if not os.path.exists(local_repository):
    from git import Repo
    tf_model_repository = 'https://github.com/tensorflow/models'
    Repo.clone_from(tf_model_repository, local_repository)
```

Now that we have downloaded the model files, we import the necessary funtions.


```python
sys.path.insert(0, 'temp/tutorials/rnn/translate/')
import seq2seq_model as seq2seq_model
import data_utils as data_utils
```

Now we start a computational graph session.


```python
sess = tf.Session()
```

We setup the model parameters for our seq2seq model.


```python
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
```

We want to download the data, save the data, save the model, and
create test english sentences for the model to be evaluated on.


```python
# Data Parameters
data_dir = 'temp'
data_file = 'eng_ger.txt'
model_path = 'seq2seq_model'
full_model_dir = os.path.join(data_dir, model_path)

# Test Translation from English (lowercase, no punct)
test_english = ['hello where is my computer',
                'the quick brown fox jumped over the lazy dog',
                'is it going to rain tomorrow']
```

Now we create the model directory if it doesn't exist.

After, we load the english-german sentence translation data.


```python
# Make Model Directory
if not os.path.exists(full_model_dir):
    os.makedirs(full_model_dir)

print('Loading English-German Data')
# Check for data, if it doesn't exist, download it and save it
if not os.path.isfile(os.path.join(data_dir, data_file)):
    print('Data not found, downloading Eng-Ger sentences from www.manythings.org')
    sentence_url = 'http://www.manythings.org/anki/deu-eng.zip'
    r = requests.get(sentence_url)
    z = ZipFile(io.BytesIO(r.content))
    file = z.read('deu.txt')
    # Format Data
    eng_ger_data = file.decode()
    eng_ger_data = eng_ger_data.encode('ascii',errors='ignore')
    eng_ger_data = eng_ger_data.decode().split('\n')
    # Write to file
    with open(os.path.join(data_dir, data_file), 'w') as out_conn:
        for sentence in eng_ger_data:
            out_conn.write(sentence + '\n')
else:
    eng_ger_data = []
    with open(os.path.join(data_dir, data_file), 'r') as in_conn:
        for row in in_conn:
            eng_ger_data.append(row[:-1])
print('Done!')
```

    Loading English-German Data


Remove punctuation and separate the english-german sentences.


```python
# Remove punctuation
eng_ger_data = [''.join(char for char in sent if char not in punct) for sent in eng_ger_data]

# Split each sentence by tabs    
eng_ger_data = [x.split('\t') for x in eng_ger_data if len(x)>=1]
[english_sentence, german_sentence] = [list(x) for x in zip(*eng_ger_data)]
english_sentence = [x.lower().split() for x in english_sentence]
german_sentence = [x.lower().split() for x in german_sentence]
```

Now we process the english, german, and test sentences.  We need to create a lookup that translates words to an index number.  Note that we purposely exclude the index 0, because that will stand for an unknown word (rare or new words).


```python
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


# Process the test english sentences, use '0' if word not in our vocab
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
print('Done!')
```

    Processing the vocabularies.
    Done!


Because the input/output sentences can be of very different lengths (1
word vs a whole sentence), we will bucket the sentences by
input/output lenghts.  For each bucket, we will train a different
seq2seq model.


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

    Data pts in bucket 0: 57222
    Data pts in bucket 1: 47613
    Data pts in bucket 2: 36158
    Data pts in bucket 3: 6795


Now we declare our seq2seq model function.


```python
# Create sequence to sequence model
def translation_model(sess, input_vocab_size, output_vocab_size,
                      buckets, rnn_size, num_layers, max_gradient,
                      learning_rate, lr_decay_rate, forward_only):
    model = seq2seq_model.Seq2SeqModel(
          input_vocab_size,
          output_vocab_size,
          buckets,
          rnn_size,
          num_layers,
          max_gradient,
          batch_size,
          learning_rate,
          lr_decay_rate,
          forward_only=forward_only,
          dtype=tf.float32)
    return(model)
```

Now we create our train/test models. We want these models to use the exact same variables, so we have to tell TensorFlow to reuse the variables when we create the test-model.


```python
print('Creating Translation Model')
input_vocab_size = vocab_size
output_vocab_size = vocab_size

translate_model = translation_model(sess=sess,
                                    input_vocab_size=vocab_size,
                                    output_vocab_size=vocab_size,
                                    buckets=buckets,
                                    rnn_size=rnn_size,
                                    num_layers=num_layers,
                                    max_gradient=max_gradient,
                                    learning_rate=learning_rate,
                                    lr_decay_rate=lr_decay_rate,
                                    forward_only=False)

# Tell TensorFlow to reuse the variables for the test model
with tf.variable_scope(tf.get_variable_scope(), reuse=True):
    #Reuse the variables for the test model
    test_model = translation_model(sess=sess,
                                    input_vocab_size=vocab_size,
                                    output_vocab_size=vocab_size,
                                    buckets=buckets,
                                    rnn_size=rnn_size,
                                    num_layers=num_layers,
                                    max_gradient=max_gradient,
                                    learning_rate=learning_rate,
                                    lr_decay_rate=lr_decay_rate,
                                    forward_only=True)
    test_model.batch_size = 1
```

    Creating Translation Model


Now we can initialize the model variables.


```python
# Initialize all model variables
init = tf.global_variables_initializer()
sess.run(init)
```

We now start the training.  For each iteration, we pick a random
bucket, and get a batch of sentences from that bucket.  There are then
trained on.


```python
# Start training
train_loss = []
for i in range(generations):
    rand_bucket_ix = np.random.choice(len(bucketed_data))
    
    model_outputs = translate_model.get_batch(bucketed_data, rand_bucket_ix)
    encoder_inputs, decoder_inputs, target_weights = model_outputs
    
    # Get the (gradient norm, loss, and outputs)
    _, step_loss, _ = translate_model.step(sess, encoder_inputs, decoder_inputs,
                                           target_weights, rand_bucket_ix, False)
    
    # Output status
    if (i+1) % output_every == 0:
        train_loss.append(step_loss)
        print('Gen #{} out of {}. Loss: {:.4}'.format(i+1, generations, step_loss))
    
    # Check if we should decay the learning rate
    if (i+1) % lr_decay_every == 0:
        sess.run(translate_model.learning_rate_decay_op)
        
    # Save model
    if (i+1) % save_every == 0:
        print('Saving model to {}.'.format(full_model_dir))
        model_save_path = os.path.join(full_model_dir, "eng_ger_translation.ckpt")
        translate_model.saver.save(sess, model_save_path, global_step=i)
        
    # Eval on test set
    if (i+1) % eval_every == 0:
        for ix, sentence in enumerate(test_data):
            # Find which bucket sentence goes in
            bucket_id = next(index for index, val in enumerate(x_maxs) if val>=len(sentence))
            # Get RNN model outputs
            encoder_inputs, decoder_inputs, target_weights = test_model.get_batch(
                {bucket_id: [(sentence, [])]}, bucket_id)
            # Get logits
            _, test_loss, output_logits = test_model.step(sess, encoder_inputs, decoder_inputs,
                                                           target_weights, bucket_id, True)
            ix_output = [int(np.argmax(logit, axis=1)) for logit in output_logits]
            # If there is a 0 symbol in outputs end the output there.
            ix_output = ix_output[0:[ix for ix, x in enumerate(ix_output+[0]) if x==0][0]]
            # Get german words from indices
            test_german = [ger_ix2vocab[x] for x in ix_output]
            print('English: {}'.format(test_english[ix]))
            print('German: {}'.format(test_german))
```

    Gen #50 out of 10000. Loss: 8.335
    Gen #100 out of 10000. Loss: 5.929
    Gen #150 out of 10000. Loss: 5.74
    Gen #200 out of 10000. Loss: 5.884
    Gen #250 out of 10000. Loss: 5.682
    Gen #300 out of 10000. Loss: 5.622
    Gen #350 out of 10000. Loss: 5.546
    Gen #400 out of 10000. Loss: 5.555
    Gen #450 out of 10000. Loss: 5.608
    Gen #500 out of 10000. Loss: 5.359
    English: hello where is my computer
    German: ['ich', 'ist', 'ist', 'ist', 'nicht', 'nicht', 'nicht', 'zu', 'zu', 'zu']
    English: the quick brown fox jumped over the lazy dog
    German: ['wir', 'ist', 'ist', 'ist', 'ist', 'nicht', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    English: is it going to rain tomorrow
    German: ['wir', 'ist', 'ist', 'ist', 'nicht', 'nicht', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    Gen #550 out of 10000. Loss: 5.308
    Gen #600 out of 10000. Loss: 5.269
    Gen #650 out of 10000. Loss: 5.234
    Gen #700 out of 10000. Loss: 5.18
    Gen #750 out of 10000. Loss: 5.351
    Gen #800 out of 10000. Loss: 5.521
    Gen #850 out of 10000. Loss: 5.276
    Gen #900 out of 10000. Loss: 4.909
    Gen #950 out of 10000. Loss: 5.045
    Gen #1000 out of 10000. Loss: 5.31
    Saving model to temp/seq2seq_model.
    English: hello where is my computer
    German: ['das', 'ist', 'ist', 'ist', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'ist', 'die', 'die', 'der', 'der', 'der', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    English: is it going to rain tomorrow
    German: ['das', 'ist', 'ist', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    Gen #1050 out of 10000. Loss: 5.326
    Gen #1100 out of 10000. Loss: 4.878
    Gen #1150 out of 10000. Loss: 4.904
    Gen #1200 out of 10000. Loss: 4.976
    Gen #1250 out of 10000. Loss: 4.717
    Gen #1300 out of 10000. Loss: 4.854
    Gen #1350 out of 10000. Loss: 5.082
    Gen #1400 out of 10000. Loss: 4.977
    Gen #1450 out of 10000. Loss: 4.844
    Gen #1500 out of 10000. Loss: 4.96
    English: hello where is my computer
    German: ['es', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'zu']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'ist', 'die', 'die', 'die', 'die', 'die', 'die', 'die', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    English: is it going to rain tomorrow
    German: ['es', 'ist', 'ist', 'ist', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    Gen #1550 out of 10000. Loss: 4.453
    Gen #1600 out of 10000. Loss: 5.135
    Gen #1650 out of 10000. Loss: 5.058
    Gen #1700 out of 10000. Loss: 4.691
    Gen #1750 out of 10000. Loss: 4.159
    Gen #1800 out of 10000. Loss: 4.77
    Gen #1850 out of 10000. Loss: 4.661
    Gen #1900 out of 10000. Loss: 4.655
    Gen #1950 out of 10000. Loss: 4.978
    Gen #2000 out of 10000. Loss: 4.655
    Saving model to temp/seq2seq_model.
    English: hello where is my computer
    German: ['das', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'ist', 'ist', 'der', 'der', 'der', 'der', 'der', 'der', 'der', 'der', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    English: is it going to rain tomorrow
    German: ['es', 'ist', 'es', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    Gen #2050 out of 10000. Loss: 4.499
    Gen #2100 out of 10000. Loss: 4.72
    Gen #2150 out of 10000. Loss: 4.111
    Gen #2200 out of 10000. Loss: 4.544
    Gen #2250 out of 10000. Loss: 3.87
    Gen #2300 out of 10000. Loss: 4.362
    Gen #2350 out of 10000. Loss: 4.252
    Gen #2400 out of 10000. Loss: 4.792
    Gen #2450 out of 10000. Loss: 4.404
    Gen #2500 out of 10000. Loss: 4.112
    English: hello where is my computer
    German: ['mein', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['die', 'ist', 'ist', 'die', 'die', 'die', 'die', 'die', 'sie', 'sie', 'sie', 'sie', 'sie', 'sie', 'sie', 'sie', 'sie']
    English: is it going to rain tomorrow
    German: ['es', 'ist', 'es', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    Gen #2550 out of 10000. Loss: 4.865
    Gen #2600 out of 10000. Loss: 3.89
    Gen #2650 out of 10000. Loss: 3.872
    Gen #2700 out of 10000. Loss: 4.871
    Gen #2750 out of 10000. Loss: 4.521
    Gen #2800 out of 10000. Loss: 4.469
    Gen #2850 out of 10000. Loss: 4.437
    Gen #2900 out of 10000. Loss: 4.789
    Gen #2950 out of 10000. Loss: 4.644
    Gen #3000 out of 10000. Loss: 4.766
    Saving model to temp/seq2seq_model.
    English: hello where is my computer
    German: ['meine', 'ist', 'ist', 'meine', 'meine', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['die', 'der', 'hat', 'die', 'der', 'der', 'der', 'der', 'die', 'die', 'die', 'die', 'die', 'die', 'die', 'die', 'die']
    English: is it going to rain tomorrow
    German: ['es', 'es', 'es', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    Gen #3050 out of 10000. Loss: 4.368
    Gen #3100 out of 10000. Loss: 4.578
    Gen #3150 out of 10000. Loss: 4.71
    Gen #3200 out of 10000. Loss: 3.402
    Gen #3250 out of 10000. Loss: 3.923
    Gen #3300 out of 10000. Loss: 4.329
    Gen #3350 out of 10000. Loss: 4.034
    Gen #3400 out of 10000. Loss: 4.64
    Gen #3450 out of 10000. Loss: 4.357
    Gen #3500 out of 10000. Loss: 4.76
    English: hello where is my computer
    German: ['mein', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'der', 'hat', 'sich', 'sich', 'die', 'die', 'die', 'die', 'die', 'die', 'war', 'war', 'war', 'war', 'war', 'war']
    English: is it going to rain tomorrow
    German: ['es', 'es', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    Gen #3550 out of 10000. Loss: 3.661
    Gen #3600 out of 10000. Loss: 3.399
    Gen #3650 out of 10000. Loss: 3.954
    Gen #3700 out of 10000. Loss: 4.573
    Gen #3750 out of 10000. Loss: 4.395
    Gen #3800 out of 10000. Loss: 4.096
    Gen #3850 out of 10000. Loss: 4.525
    Gen #3900 out of 10000. Loss: 4.456
    Gen #3950 out of 10000. Loss: 4.485
    Gen #4000 out of 10000. Loss: 3.908
    Saving model to temp/seq2seq_model.
    English: hello where is my computer
    German: ['meine', 'ist', 'meine', 'meine', 'meine', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'mann', 'wurde', 'die', 'die', 'die', 'die', 'die', 'die', 'die', 'die', 'der', 'war', 'war', 'war', 'war', 'war']
    English: is it going to rain tomorrow
    German: ['es', 'es', 'es', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    Gen #4050 out of 10000. Loss: 3.526
    Gen #4100 out of 10000. Loss: 3.986
    Gen #4150 out of 10000. Loss: 3.713
    Gen #4200 out of 10000. Loss: 3.752
    Gen #4250 out of 10000. Loss: 3.774
    Gen #4300 out of 10000. Loss: 3.7
    Gen #4350 out of 10000. Loss: 4.049
    Gen #4400 out of 10000. Loss: 3.515
    Gen #4450 out of 10000. Loss: 3.918
    Gen #4500 out of 10000. Loss: 4.466
    English: hello where is my computer
    German: ['wo', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['die', 'mann', 'hat', 'die', 'die', 'die', 'die', 'die', 'die', 'hat', 'hat', 'hat', 'hat', 'hat', 'hat', 'hat', 'hat']
    English: is it going to rain tomorrow
    German: ['ist', 'es', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    Gen #4550 out of 10000. Loss: 3.575
    Gen #4600 out of 10000. Loss: 4.065
    Gen #4650 out of 10000. Loss: 3.231
    Gen #4700 out of 10000. Loss: 3.409
    Gen #4750 out of 10000. Loss: 3.144
    Gen #4800 out of 10000. Loss: 4.42
    Gen #4850 out of 10000. Loss: 3.69
    Gen #4900 out of 10000. Loss: 4.285
    Gen #4950 out of 10000. Loss: 4.393
    Gen #5000 out of 10000. Loss: 4.216
    Saving model to temp/seq2seq_model.
    English: hello where is my computer
    German: ['wo', 'ist', 'meine', 'meine', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'mann', 'hat', 'die', 'die', 'die', 'die', 'die', 'die', 'die', 'die', 'war', 'war', 'hat', 'hat', 'hat', 'hat']
    English: is it going to rain tomorrow
    German: ['ist', 'es', 'morgen', 'morgen', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    Gen #5050 out of 10000. Loss: 3.256
    Gen #5100 out of 10000. Loss: 3.782
    Gen #5150 out of 10000. Loss: 3.63
    Gen #5200 out of 10000. Loss: 3.249
    Gen #5250 out of 10000. Loss: 3.444
    Gen #5300 out of 10000. Loss: 4.069
    Gen #5350 out of 10000. Loss: 3.32
    Gen #5400 out of 10000. Loss: 4.405
    Gen #5450 out of 10000. Loss: 3.054
    Gen #5500 out of 10000. Loss: 3.439
    English: hello where is my computer
    German: ['wo', 'ist', 'meine', 'meine', 'meine', 'meine', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'mann', 'hat', 'die', 'die', 'die', 'die', 'die', 'den', 'den', 'den', 'zu', 'hatte', 'hat', 'hat', 'hat', 'hat']
    English: is it going to rain tomorrow
    German: ['ist', 'es', 'morgen', 'morgen', 'morgen', 'morgen', 'zu', 'zu', 'zu', 'zu', 'zu', 'zu']
    Gen #5550 out of 10000. Loss: 3.686
    Gen #5600 out of 10000. Loss: 3.612
    Gen #5650 out of 10000. Loss: 2.975
    Gen #5700 out of 10000. Loss: 4.207
    Gen #5750 out of 10000. Loss: 4.103
    Gen #5800 out of 10000. Loss: 3.184
    Gen #5850 out of 10000. Loss: 4.118
    Gen #5900 out of 10000. Loss: 3.51
    Gen #5950 out of 10000. Loss: 3.406
    Gen #6000 out of 10000. Loss: 4.096
    Saving model to temp/seq2seq_model.
    English: hello where is my computer
    German: ['wo', 'ist', 'meine', 'meine', 'mein', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'mann', 'hat', 'die', 'die', 'die', 'die', 'die', 'die', 'die', 'hatte', 'hatte', 'hatte', 'hatte', 'hatte', 'hatte', 'hatte']
    English: is it going to rain tomorrow
    German: ['ist', 'es', 'morgen', 'morgen', 'morgen', 'morgen', 'morgen', 'zu', 'zu', 'zu', 'zu', 'zu']
    Gen #6050 out of 10000. Loss: 3.151
    Gen #6100 out of 10000. Loss: 3.761
    Gen #6150 out of 10000. Loss: 3.79
    Gen #6200 out of 10000. Loss: 3.692
    Gen #6250 out of 10000. Loss: 4.191
    Gen #6300 out of 10000. Loss: 3.054
    Gen #6350 out of 10000. Loss: 3.479
    Gen #6400 out of 10000. Loss: 3.183
    Gen #6450 out of 10000. Loss: 3.441
    Gen #6500 out of 10000. Loss: 4.02
    English: hello where is my computer
    German: ['wo', 'ist', 'meine', 'meine', 'meine', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'mann', 'hat', 'den', 'den', 'den', 'den', 'den', 'den', 'den', 'den', 'der', 'war', 'war', 'war', 'war', 'war']
    English: is it going to rain tomorrow
    German: ['wird', 'es', 'morgen', 'morgen', 'morgen', 'morgen', 'morgen', 'morgen', 'gehen', 'gehen', 'gehen', 'gehen']
    Gen #6550 out of 10000. Loss: 3.493
    Gen #6600 out of 10000. Loss: 3.04
    Gen #6650 out of 10000. Loss: 3.078
    Gen #6700 out of 10000. Loss: 3.51
    Gen #6750 out of 10000. Loss: 3.814
    Gen #6800 out of 10000. Loss: 3.468
    Gen #6850 out of 10000. Loss: 3.268
    Gen #6900 out of 10000. Loss: 4.03
    Gen #6950 out of 10000. Loss: 3.029
    Gen #7000 out of 10000. Loss: 3.59
    Saving model to temp/seq2seq_model.
    English: hello where is my computer
    German: ['wo', 'ist', 'meine', 'meine', 'meine', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'mann', 'hat', 'sich', 'die', 'die', 'die', 'mann', 'mann', 'mann', 'mann', 'hat', 'hat', 'hat', 'hat', 'hat', 'hat']
    English: is it going to rain tomorrow
    German: ['wird', 'es', 'morgen', 'morgen', 'morgen', 'morgen', 'gehen', 'gehen', 'gehen', 'werden', 'werden', 'werden']
    Gen #7050 out of 10000. Loss: 2.631
    Gen #7100 out of 10000. Loss: 2.974
    Gen #7150 out of 10000. Loss: 3.306
    Gen #7200 out of 10000. Loss: 3.555
    Gen #7250 out of 10000. Loss: 3.943
    Gen #7300 out of 10000. Loss: 3.932
    Gen #7350 out of 10000. Loss: 3.583
    Gen #7400 out of 10000. Loss: 3.593
    Gen #7450 out of 10000. Loss: 2.833
    Gen #7500 out of 10000. Loss: 3.738
    English: hello where is my computer
    German: ['wo', 'ist', 'mein', 'mein', 'mein', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'mann', 'hat', 'sich', 'die', 'die', 'die', 'die', 'hund', 'hund', 'hund', 'hund', 'hund', 'wurde', 'wurde', 'wurde', 'wurde']
    English: is it going to rain tomorrow
    German: ['wird', 'es', 'morgen', 'morgen', 'morgen', 'kommen', 'kommen', 'kommen', 'kommen', 'kommen', 'kommen', 'kommen']
    Gen #7550 out of 10000. Loss: 3.515
    Gen #7600 out of 10000. Loss: 2.954
    Gen #7650 out of 10000. Loss: 3.933
    Gen #7700 out of 10000. Loss: 3.252
    Gen #7750 out of 10000. Loss: 2.993
    Gen #7800 out of 10000. Loss: 3.597
    Gen #7850 out of 10000. Loss: 3.499
    Gen #7900 out of 10000. Loss: 4.009
    Gen #7950 out of 10000. Loss: 3.716
    Gen #8000 out of 10000. Loss: 2.477
    Saving model to temp/seq2seq_model.
    English: hello where is my computer
    German: ['wo', 'ist', 'mein', 'mein', 'mein', 'mein', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'mann', 'hat', 'sich', 'die', 'die', 'die', 'hund', 'hund', 'hund', 'hund', 'wurde', 'wurde', 'wurde', 'wurde', 'wurde', 'wurde']
    English: is it going to rain tomorrow
    German: ['ist', 'es', 'morgen', 'morgen', 'morgen', 'kommen', 'kommen', 'kommen', 'kommen', 'kommen', 'kommen', 'kommen']
    Gen #8050 out of 10000. Loss: 3.264
    Gen #8100 out of 10000. Loss: 2.105
    Gen #8150 out of 10000. Loss: 3.692
    Gen #8200 out of 10000. Loss: 2.701
    Gen #8250 out of 10000. Loss: 3.574
    Gen #8300 out of 10000. Loss: 2.88
    Gen #8350 out of 10000. Loss: 3.554
    Gen #8400 out of 10000. Loss: 3.779
    Gen #8450 out of 10000. Loss: 2.547
    Gen #8500 out of 10000. Loss: 3.565
    English: hello where is my computer
    German: ['wo', 'ist', 'meine', 'mein', 'mein', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'mann', 'hat', 'sich', 'sich', 'die', 'die', 'die', 'hund', 'hund', 'hund', 'hund', 'hund', 'hat', 'hat', 'hat', 'hat']
    English: is it going to rain tomorrow
    German: ['wird', 'es', 'morgen', 'morgen', 'morgen', 'kommen', 'kommen', 'kommen', 'kommen', 'kommen', 'kommen', 'muss']
    Gen #8550 out of 10000. Loss: 2.268
    Gen #8600 out of 10000. Loss: 2.799
    Gen #8650 out of 10000. Loss: 2.679
    Gen #8700 out of 10000. Loss: 3.278
    Gen #8750 out of 10000. Loss: 3.373
    Gen #8800 out of 10000. Loss: 2.632
    Gen #8850 out of 10000. Loss: 3.302
    Gen #8900 out of 10000. Loss: 3.145
    Gen #8950 out of 10000. Loss: 2.752
    Gen #9000 out of 10000. Loss: 3.807
    Saving model to temp/seq2seq_model.
    English: hello where is my computer
    German: ['wo', 'ist', 'meine', 'meine', 'mein', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'mann', 'hat', 'sich', 'sich', 'die', 'die', 'die', 'die', 'die', 'hund', 'hund', 'hund', 'hund', 'hat', 'hat', 'hat']
    English: is it going to rain tomorrow
    German: ['wird', 'es', 'morgen', 'morgen', 'morgen', 'morgen', 'kommen', 'kommen', 'kommen', 'kommen', 'kommen', 'kommen']
    Gen #9050 out of 10000. Loss: 2.773
    Gen #9100 out of 10000. Loss: 2.477
    Gen #9150 out of 10000. Loss: 2.899
    Gen #9200 out of 10000. Loss: 2.985
    Gen #9250 out of 10000. Loss: 2.741
    Gen #9300 out of 10000. Loss: 3.437
    Gen #9350 out of 10000. Loss: 2.529
    Gen #9400 out of 10000. Loss: 3.025
    Gen #9450 out of 10000. Loss: 3.395
    Gen #9500 out of 10000. Loss: 2.775
    English: hello where is my computer
    German: ['wo', 'ist', 'meine', 'meine', 'ist', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'mann', 'hat', 'sich', 'ber', 'die', 'die', 'hund', 'hund', 'hund', 'hund', 'hund', 'hund', 'hund', 'hund', 'hund', 'hund']
    English: is it going to rain tomorrow
    German: ['wird', 'es', 'morgen', 'morgen', 'morgen', 'morgen', 'kommen', 'kommen', 'kommen', 'kommen', 'kommen', 'muss']
    Gen #9550 out of 10000. Loss: 2.758
    Gen #9600 out of 10000. Loss: 2.622
    Gen #9650 out of 10000. Loss: 3.466
    Gen #9700 out of 10000. Loss: 2.492
    Gen #9750 out of 10000. Loss: 2.718
    Gen #9800 out of 10000. Loss: 3.33
    Gen #9850 out of 10000. Loss: 2.351
    Gen #9900 out of 10000. Loss: 2.164
    Gen #9950 out of 10000. Loss: 3.074
    Gen #10000 out of 10000. Loss: 2.125
    Saving model to temp/seq2seq_model.
    English: hello where is my computer
    German: ['wo', 'wo', 'mein', 'mein', 'mein', 'ist', 'ist', 'ist', 'ist', 'ist']
    English: the quick brown fox jumped over the lazy dog
    German: ['der', 'mann', 'hat', 'sich', 'ber', 'ber', 'den', 'hund', 'hund', 'hund', 'hund', 'hund', 'hund', 'hat', 'hat', 'hat', 'hat']
    English: is it going to rain tomorrow
    German: ['wird', 'es', 'morgen', 'morgen', 'morgen', 'regnen', 'kommen', 'kommen', 'kommen', 'kommen', 'kommen', 'kommen']


Here is matplotib code to plot the training loss over the 10000 iterations.


```python
# Plot train loss
loss_generations = [i for i in range(generations) if i%output_every==0]
plt.plot(loss_generations, train_loss, 'k-')
plt.title('Sequence to Sequence Loss')
plt.xlabel('Generation')
plt.ylabel('Loss')
plt.show()
```


![png](output_29_0.png)

