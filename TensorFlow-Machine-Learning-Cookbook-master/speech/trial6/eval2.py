from __future__ import absolute_import
from __future__ import division
from tensorflow.contrib.framework.python.ops import audio_ops as contrib_audio
from tensorflow.python.ops import io_ops
from tensorflow.python.platform import gfile
from tensorflow.python.util import compat
import tensorflow as tf
import hashlib, math, os.path, random, re, sys
import numpy as np
from six.moves import urllib
from six.moves import xrange
import tensorflow as tf
from six.moves import xrange
import train2

FLAGS = None
#wanted_words = ['down','up']
wanted_words = ['down','go','left','no','off','on','right','stop','up','yes']

MAX_NUM_WAVS_PER_CLASS = 2**27 - 1  # ~134M
SILENCE_LABEL = '_silence_'
SILENCE_INDEX = 0
UNKNOWN_WORD_LABEL = '_unknown_'
UNKNOWN_WORD_INDEX = 1
BACKGROUND_NOISE_DIR_NAME = '_background_noise_'
RANDOM_SEED = 59185

def prepare_words_list(wanted_words):
  return [SILENCE_LABEL, UNKNOWN_WORD_LABEL] + wanted_words

num_cell = 256
silence_percentage = 10.0
unknown_percentage = 50.0
check_nans = False
train_dir = '/tmp/speech_commands_train'
save_step_interval = 100
summaries_dir = '/tmp/retrain_logs'
time_shift_ms = 100.0
background_volume_range = 0.1
background_frequency = 0.8
batch_size = 100
sample_rate = 16000
clip_duration_ms = 1000
window_size_ms = 30.0
window_stride_ms = 10.0
validation_percentage=10.0
testing_percentage=10.0
desired_samples = int(sample_rate * clip_duration_ms / 1000)
window_size_samples = int(sample_rate * window_size_ms / 1000)
window_stride_samples = int(sample_rate * window_stride_ms / 1000)
length_minus_window = (desired_samples - window_size_samples)
if length_minus_window < 0:
  spectrogram_length = 0
else:
  spectrogram_length = 1 + int(length_minus_window / window_stride_samples)
dct_coefficient_count = 40
fingerprint_size = dct_coefficient_count * spectrogram_length
label_count = len(prepare_words_list(wanted_words))  
how_many_training_steps = "15000,3000"
learning_rate = "0.001,0.0001"
training_steps_list = list(map(int, how_many_training_steps.split(',') ))
learning_rates_list = list(map(float, learning_rate.split(',')))

def which_set(filename, validation_percentage, testing_percentage):
  base_name = os.path.basename(filename)
  hash_name = re.sub(r'_nohash_.*$', '', base_name)
  hash_name_hashed = hashlib.sha1(compat.as_bytes(hash_name)).hexdigest()
  percentage_hash = ((int(hash_name_hashed, 16) %
                      (MAX_NUM_WAVS_PER_CLASS + 1)) *
                     (100.0 / MAX_NUM_WAVS_PER_CLASS))
  if percentage_hash < validation_percentage:
    result = 'validation'
  elif percentage_hash < (testing_percentage + validation_percentage):
    result = 'testing'
  else:
    result = 'training'
  return result

class AudioProcessor(object):

  def __init__(self, data_dir):
    self.data_dir = data_dir
    self.prepare_data_index()
    self.prepare_processing_graph()

  def prepare_data_index(self):
    search_path = os.path.join(self.data_dir, '*', '*.wav')
    self.files = []
    for wav_path in gfile.Glob(search_path):
      self.files.append(wav_path)
    #print self.files


  def prepare_processing_graph(self):
    self.wav_filename_placeholder_ = tf.placeholder(tf.string, [])
    wav_loader = io_ops.read_file(self.wav_filename_placeholder_)
    wav_decoder = contrib_audio.decode_wav(
        wav_loader, desired_channels=1, desired_samples=desired_samples)
    spectrogram = contrib_audio.audio_spectrogram(
        wav_decoder.audio,
        window_size=window_size_samples,
        stride=window_stride_samples,
        magnitude_squared=True)
    print 'spectrogram', spectrogram
    print 'dct_coefficient_count', dct_coefficient_count
    print 'wav_decoder.sample_rate', wav_decoder.sample_rate
    self.mfcc_ = contrib_audio.mfcc(
        spectrogram,
        wav_decoder.sample_rate,
        dct_coefficient_count=dct_coefficient_count)
    print 'self.mfcc_', self.mfcc_

  def set_size(self, mode):
    return len(self.data_index[mode])

  def get_data(self, sess, sample_i):
    # Pick one of the partitions to choose samples from.
    #print len(self.files)
    candidates = self.files
    # Data and labels will be populated and returned.
    data = np.zeros((1, fingerprint_size))
    labels = np.zeros((1, label_count))
    # Use the processing graph we created earlier to repeatedly to generate the
    # final output sample data we'll use in training.
    sample = candidates[sample_i]
    #print sample
    label = re.findall(".*/(.*?)/.*?.wav",sample)[0]
    input_dict = {
        self.wav_filename_placeholder_: sample
    }
    data[0, :] = sess.run(self.mfcc_, feed_dict=input_dict).flatten()
    if label not in wanted_words:
      labels[0, 1] = 1
    else:
      labels[0, prepare_words_list(wanted_words).index(label)] = 1
    return data, labels

def load_variables_from_checkpoint(sess, start_checkpoint):
  saver = tf.train.Saver(tf.global_variables())
  saver.restore(sess, start_checkpoint)



def main(_):
  
  # We want to see all the logging messages for this tutorial.
  tf.logging.set_verbosity(tf.logging.INFO)

  # Start a new TensorFlow session.
  sess = tf.InteractiveSession()
    
  audio_processor = AudioProcessor(
    #"/home/burak/Downloads/train/audio", 
    "/home/burak/Downloads/test/audio"
    )
  
  fingerprint_input = tf.placeholder(tf.float32,[None, fingerprint_size])

  print 'fingerprint_input',fingerprint_input

  logits = train2.create_conv_model(fingerprint_input, is_training=False)
       
  predicted_indices = tf.argmax(logits, 1)

  saver = tf.train.Saver(tf.global_variables())

  # Merge all the summaries and write them out to /tmp/retrain_logs (by default)
  merged_summaries = tf.summary.merge_all()
    
  tf.global_variables_initializer().run()
  
  print 'restoring'
  load_variables_from_checkpoint(sess, "/home/burak/Downloads/model_2/conv.ckpt-18000")

  accuracy = 0
  N = 660
  for i in range(N):
    x, y = audio_processor.get_data(sess, i)
    #print x.shape, y.shape
    pred = sess.run([predicted_indices], feed_dict={fingerprint_input: x})[0]
    #print y, np.argmax(y[0]), pred
    if pred[0] == np.argmax(y[0]): accuracy+=1.0

  print accuracy, accuracy / N


if __name__ == '__main__':
  tf.app.run(main=main, argv=[sys.argv[0]])
