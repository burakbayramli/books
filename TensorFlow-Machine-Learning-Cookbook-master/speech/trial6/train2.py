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
time_shift_samples = int((time_shift_ms * sample_rate) / 1000)
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
    self.prepare_background_data()
    self.prepare_processing_graph()


  def prepare_data_index(self):
    # Make sure the shuffling and picking of unknowns is deterministic.
    random.seed(RANDOM_SEED)
    wanted_words_index = {}
    for index, wanted_word in enumerate(wanted_words):
      wanted_words_index[wanted_word] = index + 2
    self.data_index = {'validation': [], 'testing': [], 'training': []}
    unknown_index = {'validation': [], 'testing': [], 'training': []}
    all_words = {}
    # Look through all the subfolders to find audio samples
    search_path = os.path.join(self.data_dir, '*', '*.wav')
    for wav_path in gfile.Glob(search_path):
      word = re.search('.*/([^/]+)/.*.wav', wav_path).group(1).lower()
      # Treat the '_background_noise_' folder as a special case, since we expect
      # it to contain long audio samples we mix in to improve training.
      if word == BACKGROUND_NOISE_DIR_NAME:
        continue
      all_words[word] = True
      set_index = which_set(wav_path, validation_percentage, testing_percentage)
      # If it's a known class, store its detail, otherwise add it to the list
      # we'll use to train the unknown label.
      if word in wanted_words_index:
        self.data_index[set_index].append({'label': word, 'file': wav_path})
      else:
        unknown_index[set_index].append({'label': word, 'file': wav_path})
    if not all_words:
      raise Exception('No .wavs found at ' + search_path)
    for index, wanted_word in enumerate(wanted_words):
      if wanted_word not in all_words:
        raise Exception('Expected to find ' + wanted_word +
                        ' in labels but only found ' +
                        ', '.join(all_words.keys()))
    # We need an arbitrary file to load as the input for the silence samples.
    # It's multiplied by zero later, so the content doesn't matter.
    silence_wav_path = self.data_index['training'][0]['file']
    for set_index in ['validation', 'testing', 'training']:
      set_size = len(self.data_index[set_index])
      silence_size = int(math.ceil(set_size * silence_percentage / 100))
      for _ in range(silence_size):
        self.data_index[set_index].append({
            'label': SILENCE_LABEL,
            'file': silence_wav_path
        })
      # Pick some unknowns to add to each partition of the data set.
      random.shuffle(unknown_index[set_index])
      unknown_size = int(math.ceil(set_size * unknown_percentage / 100))
      self.data_index[set_index].extend(unknown_index[set_index][:unknown_size])
    # Make sure the ordering is random.
    for set_index in ['validation', 'testing', 'training']:
      random.shuffle(self.data_index[set_index])
    # Prepare the rest of the result data structure.
    self.words_list = prepare_words_list(wanted_words)
    self.word_to_index = {}
    for word in all_words:
      if word in wanted_words_index:
        self.word_to_index[word] = wanted_words_index[word]
      else:
        self.word_to_index[word] = UNKNOWN_WORD_INDEX
    self.word_to_index[SILENCE_LABEL] = SILENCE_INDEX

  def prepare_background_data(self):
    self.background_data = []
    background_dir = os.path.join(self.data_dir, BACKGROUND_NOISE_DIR_NAME)
    if not os.path.exists(background_dir):
      return self.background_data
    with tf.Session(graph=tf.Graph()) as sess:
      wav_filename_placeholder = tf.placeholder(tf.string, [])
      wav_loader = io_ops.read_file(wav_filename_placeholder)
      wav_decoder = contrib_audio.decode_wav(wav_loader, desired_channels=1)
      search_path = os.path.join(self.data_dir, BACKGROUND_NOISE_DIR_NAME,
                                 '*.wav')
      for wav_path in gfile.Glob(search_path):
        wav_data = sess.run(
            wav_decoder,
            feed_dict={wav_filename_placeholder: wav_path}).audio.flatten()
        self.background_data.append(wav_data)
      if not self.background_data:
        raise Exception('No background wav files were found in ' + search_path)

  def prepare_processing_graph(self):
    self.wav_filename_placeholder_ = tf.placeholder(tf.string, [])
    wav_loader = io_ops.read_file(self.wav_filename_placeholder_)
    wav_decoder = contrib_audio.decode_wav(
        wav_loader, desired_channels=1, desired_samples=desired_samples)
    # Allow the audio sample's volume to be adjusted.
    self.foreground_volume_placeholder_ = tf.placeholder(tf.float32, [])
    scaled_foreground = tf.multiply(wav_decoder.audio,
                                    self.foreground_volume_placeholder_)
    # Shift the sample's start position, and pad any gaps with zeros.
    self.time_shift_padding_placeholder_ = tf.placeholder(tf.int32, [2, 2])
    self.time_shift_offset_placeholder_ = tf.placeholder(tf.int32, [2])
    padded_foreground = tf.pad(
        scaled_foreground,
        self.time_shift_padding_placeholder_,
        mode='CONSTANT')
    sliced_foreground = tf.slice(padded_foreground,
                                 self.time_shift_offset_placeholder_,
                                 [desired_samples, -1])
    # Mix in background noise.
    self.background_data_placeholder_ = tf.placeholder(tf.float32,[desired_samples, 1])
    self.background_volume_placeholder_ = tf.placeholder(tf.float32, [])
    background_mul = tf.multiply(self.background_data_placeholder_,
                                 self.background_volume_placeholder_)
    background_add = tf.add(background_mul, sliced_foreground)
    background_clamp = tf.clip_by_value(background_add, -1.0, 1.0)
    # Run the spectrogram and MFCC ops to get a 2D 'fingerprint' of the audio.
    print 'window_size_samples', window_size_samples
    print 'window_stride_samples', window_stride_samples
    print 'background_clamp', background_clamp
    spectrogram = contrib_audio.audio_spectrogram(
        background_clamp,
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

  def get_data(self, offset, mode, sess):
    time_shift = time_shift_samples
    how_many = batch_size
    # Pick one of the partitions to choose samples from.
    candidates = self.data_index[mode]
    if how_many == -1:
      sample_count = len(candidates)
    else:
      sample_count = max(0, min(how_many, len(candidates) - offset))
    # Data and labels will be populated and returned.
    data = np.zeros((sample_count, fingerprint_size))
    labels = np.zeros((sample_count, label_count))
    use_background = self.background_data and (mode == 'training')
    pick_deterministically = (mode != 'training')
    # Use the processing graph we created earlier to repeatedly to generate the
    # final output sample data we'll use in training.
    for i in xrange(offset, offset + sample_count):
      # Pick which audio sample to use.
      if how_many == -1 or pick_deterministically:
        sample_index = i
      else:
        sample_index = np.random.randint(len(candidates))
      sample = candidates[sample_index]
      # If we're time shifting, set up the offset for this sample.
      if time_shift > 0:
        time_shift_amount = np.random.randint(-time_shift, time_shift)
      else:
        time_shift_amount = 0
      if time_shift_amount > 0:
        time_shift_padding = [[time_shift_amount, 0], [0, 0]]
        time_shift_offset = [0, 0]
      else:
        time_shift_padding = [[0, -time_shift_amount], [0, 0]]
        time_shift_offset = [-time_shift_amount, 0]
      input_dict = {
          self.wav_filename_placeholder_: sample['file'],
          self.time_shift_padding_placeholder_: time_shift_padding,
          self.time_shift_offset_placeholder_: time_shift_offset,
      }
      # Choose a section of background noise to mix in.
      if use_background:
        background_index = np.random.randint(len(self.background_data))
        background_samples = self.background_data[background_index]
        background_offset = np.random.randint(
            0, len(background_samples) - desired_samples)
        background_clipped = background_samples[background_offset:(
            background_offset + desired_samples)]
        background_reshaped = background_clipped.reshape([desired_samples, 1])
        if np.random.uniform(0, 1) < background_frequency:
          background_volume = np.random.uniform(0, background_volume_range)
        else:
          background_volume = 0
      else:
        background_reshaped = np.zeros([desired_samples, 1])
        background_volume = 0
      input_dict[self.background_data_placeholder_] = background_reshaped
      input_dict[self.background_volume_placeholder_] = background_volume
      # If we want silence, mute out the main sample but leave the background.
      if sample['label'] == SILENCE_LABEL:
        input_dict[self.foreground_volume_placeholder_] = 0
      else:
        input_dict[self.foreground_volume_placeholder_] = 1
      # Run the graph to produce the output audio.
      data[i - offset, :] = sess.run(self.mfcc_, feed_dict=input_dict).flatten()
      label_index = self.word_to_index[sample['label']]
      labels[i - offset, label_index] = 1
    return data, labels

def load_variables_from_checkpoint(sess, start_checkpoint):
  saver = tf.train.Saver(tf.global_variables())
  saver.restore(sess, start_checkpoint)


def create_conv_model(fingerprint_input, is_training):
  if is_training:
    dropout_prob = tf.placeholder(tf.float32, name='dropout_prob')
  input_frequency_size = dct_coefficient_count
  input_time_size = spectrogram_length
  fingerprint_4d = tf.reshape(fingerprint_input,
                              [-1, input_time_size, input_frequency_size, 1])

  print 'fingerprint_4d', fingerprint_4d
  first_filter_width = 8
  first_filter_height = 20
  first_filter_count = 64
  first_weights = tf.Variable(
      tf.truncated_normal(
          [first_filter_height, first_filter_width, 1, first_filter_count],
          stddev=0.01))
  first_bias = tf.Variable(tf.zeros([first_filter_count]))
  first_conv = tf.nn.conv2d(fingerprint_4d, first_weights, [1, 1, 1, 1],
                            'SAME') + first_bias
  print 'first_conv',first_conv
  first_relu = tf.nn.relu(first_conv)
  if is_training:
    first_dropout = tf.nn.dropout(first_relu, dropout_prob)
  else:
    first_dropout = first_relu
  max_pool = tf.nn.max_pool(first_dropout, [1, 2, 2, 1], [1, 2, 2, 1], 'SAME')
  print max_pool
  second_filter_width = 4
  second_filter_height = 10
  second_filter_count = 64
  second_weights = tf.Variable(
      tf.truncated_normal(
          [
              second_filter_height, second_filter_width, first_filter_count,
              second_filter_count
          ],
          stddev=0.01))
  second_bias = tf.Variable(tf.zeros([second_filter_count]))
  second_conv = tf.nn.conv2d(max_pool, second_weights, [1, 1, 1, 1],
                             'SAME') + second_bias
  print 'second_conv', second_conv
  second_relu = tf.nn.relu(second_conv)
  if is_training:
    second_dropout = tf.nn.dropout(second_relu, dropout_prob)
  else:
    second_dropout = second_relu
  second_conv_shape = second_dropout.get_shape()
  second_conv_output_width = second_conv_shape[2]
  second_conv_output_height = second_conv_shape[1]
  second_conv_element_count = int(
      second_conv_output_width * second_conv_output_height *
      second_filter_count)
  flattened_second_conv = tf.reshape(second_dropout,
                                     [-1, second_conv_element_count])
  print 'flattened_second_conv', flattened_second_conv
  final_fc_weights = tf.Variable(
      tf.truncated_normal(
          [second_conv_element_count, label_count], stddev=0.01))
  final_fc_bias = tf.Variable(tf.zeros([label_count]))
  final_fc = tf.matmul(flattened_second_conv, final_fc_weights) + final_fc_bias
  print 'final_fc',final_fc
  
  if is_training:
    return final_fc, dropout_prob
  else:
    return final_fc


def main(_):
  
  # We want to see all the logging messages for this tutorial.
  tf.logging.set_verbosity(tf.logging.INFO)

  # Start a new TensorFlow session.
  sess = tf.InteractiveSession()
    
  audio_processor = AudioProcessor(
    "/home/burak/Downloads/train/audio", 
    #"/home/burak/Downloads/test/audio"
    )

  fingerprint_input = tf.placeholder(
      tf.float32, [None, fingerprint_size], name='fingerprint_input')

  print 'fingerprint_input',fingerprint_input

  logits, dropout_prob = create_conv_model(fingerprint_input, is_training=True)
  

  # Define loss and optimizer
  ground_truth_input = tf.placeholder(
      tf.float32, [None, label_count], name='groundtruth_input')

  # Optionally we can add runtime checks to spot when NaNs or other symptoms of
  # numerical errors start occurring during training.
  control_dependencies = []
  if check_nans:
    checks = tf.add_check_numerics_ops()
    control_dependencies = [checks]

  # Create the back propagation and training evaluation machinery in the graph.
  cross_entropy_mean = tf.reduce_mean(
    tf.nn.softmax_cross_entropy_with_logits(
      labels=ground_truth_input, logits=logits))
  
  tf.summary.scalar('cross_entropy', cross_entropy_mean)
  
  with tf.name_scope('train'), tf.control_dependencies(control_dependencies):
    learning_rate_input = tf.placeholder(
        tf.float32, [], name='learning_rate_input')
    train_step = tf.train.GradientDescentOptimizer(
        learning_rate_input).minimize(cross_entropy_mean)
    
  predicted_indices = tf.argmax(logits, 1)
  expected_indices = tf.argmax(ground_truth_input, 1)
  correct_prediction = tf.equal(predicted_indices, expected_indices)
  evaluation_step = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))
  tf.summary.scalar('accuracy', evaluation_step)
  global_step = tf.contrib.framework.get_or_create_global_step()
  increment_global_step = tf.assign(global_step, global_step + 1)

  saver = tf.train.Saver(tf.global_variables())

  # Merge all the summaries and write them out to /tmp/retrain_logs (by default)
  merged_summaries = tf.summary.merge_all()
  
  train_writer = tf.summary.FileWriter(summaries_dir + '/train',sess.graph)
  
  validation_writer = tf.summary.FileWriter(summaries_dir + '/validation')

  tf.global_variables_initializer().run()

  start_step = 1

#  if FLAGS.start_checkpoint:
#    print 'restoring',
#    load_variables_from_checkpoint(sess, FLAGS.start_checkpoint)
#    start_step = global_step.eval(session=sess)

  tf.logging.info('Training from step: %d ', start_step)

  # Save graph.pbtxt.
  tf.train.write_graph(sess.graph_def, train_dir, 'conv.pbtxt')

  # Save list of words.
  with gfile.GFile(
      os.path.join(train_dir, 'conv_labels.txt'),
      'w') as f:
    f.write('\n'.join(audio_processor.words_list))

  # Training loop.
  training_steps_max = np.sum(training_steps_list)
  for training_step in xrange(start_step, training_steps_max + 1):
    # Figure out what the current learning rate is.
    training_steps_sum = 0
    for i in range(len(training_steps_list)):
      training_steps_sum += training_steps_list[i]
      if training_step <= training_steps_sum:
        learning_rate_value = learning_rates_list[i]
        break
    # Pull the audio samples we'll use for training.
    train_fingerprints, train_ground_truth = audio_processor.get_data(0,  'training', sess)
    # Run the graph with this batch of training data.
    train_summary, train_accuracy, cross_entropy_value, _, _ = sess.run(
        [
            merged_summaries, evaluation_step, cross_entropy_mean, train_step,
            increment_global_step
        ],
        feed_dict={
            fingerprint_input: train_fingerprints,
            ground_truth_input: train_ground_truth,
            learning_rate_input: learning_rate_value,
            dropout_prob: 0.5
        })
    train_writer.add_summary(train_summary, training_step)
    tf.logging.info('Step #%d: rate %f, accuracy %.1f%%, cross entropy %f' %
                    (training_step, learning_rate_value, train_accuracy * 100,
                     cross_entropy_value))

    # Save the model checkpoint periodically.
    if (training_step % save_step_interval == 0 or
        training_step == training_steps_max):
      checkpoint_path = os.path.join(train_dir, 'conv.ckpt')
      tf.logging.info('Saving to "%s-%d"', checkpoint_path, training_step)
      saver.save(sess, checkpoint_path, global_step=training_step)


if __name__ == '__main__':
  tf.app.run(main=main, argv=[sys.argv[0]])
