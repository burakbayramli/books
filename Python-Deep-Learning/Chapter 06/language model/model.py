# -*- coding: utf-8 -*-
from __future__ import print_function, division

import time
import codecs
import locale
import sys
import numpy as np
import tensorflow as tf
import data_reader

class Model(object):
    """RNN language model."""
    def __init__(self, batch_size, sequence_length, lstm_sizes, dropout,
                 labels, save_path):
        self.batch_size = batch_size
        self.sequence_length = sequence_length
        self.lstm_sizes = lstm_sizes
        self.labels = labels
        self.label_map = {val: idx for idx, val in enumerate(labels)}
        self.number_of_characters = len(labels)
        self.save_path = save_path
        self.dropout = dropout

    def init_graph(self):
        # Variable sequence length
        self.inputs = tf.placeholder(
            tf.int32, [self.batch_size, self.sequence_length])
        self.targets = tf.placeholder(
            tf.int32, [self.batch_size, self.sequence_length])
        self.init_architecture()
        self.saver = tf.train.Saver(tf.trainable_variables())

    def init_architecture(self):
        # Define a multilayer LSTM cell
        self.one_hot_inputs = tf.one_hot(
            self.inputs, depth=self.number_of_characters)
        cell_list = [tf.nn.rnn_cell.LSTMCell(lstm_size, state_is_tuple=True)
                     for lstm_size in self.lstm_sizes]
        self.multi_cell_lstm = tf.nn.rnn_cell.MultiRNNCell(
            cell_list, state_is_tuple=True)
        # Initial state of the LSTM memory.
        # Keep state in graph memory to use between batches
        self.initial_state = self.multi_cell_lstm.zero_state(
            self.batch_size, tf.float32)
        # Convert to variables so that the state can be stored between batches
        # Note that LSTM states is a tuple of tensors, this structure has to be
        # re-created in order to use as LSTM state.
        self.state_variables = tf.python.util.nest.pack_sequence_as(
            self.initial_state,
            [tf.Variable(var, trainable=False)
             for var in tf.python.util.nest.flatten(self.initial_state)])
        # Define the rnn through time
        lstm_output, final_state = tf.nn.dynamic_rnn(
            cell=self.multi_cell_lstm, inputs=self.one_hot_inputs,
            initial_state=self.state_variables)
        # Force the initial state to be set to the new state for the next batch
        # before returning the output
        store_states = [
            state_variable.assign(new_state)
            for (state_variable, new_state) in zip(
                tf.python.util.nest.flatten(self.state_variables),
                tf.python.util.nest.flatten(final_state))]
        with tf.control_dependencies(store_states):
            lstm_output = tf.identity(lstm_output)
        # Reshape so that we can apply the linear transformation to all outputs
        output_flat = tf.reshape(lstm_output, (-1, self.lstm_sizes[-1]))
        # Define output layer
        self.logit_weights = tf.Variable(
            tf.truncated_normal(
                (self.lstm_sizes[-1], self.number_of_characters), stddev=0.01),
            name='logit_weights')
        self.logit_bias = tf.Variable(
            tf.zeros((self.number_of_characters)), name='logit_bias')
        # Apply last layer transformation
        self.logits_flat = tf.matmul(
            output_flat, self.logit_weights) + self.logit_bias
        probabilities_flat = tf.nn.softmax(self.logits_flat)
        self.probabilities = tf.reshape(
            probabilities_flat,
            (self.batch_size, -1, self.number_of_characters))

    def init_train_op(self, optimizer):
        # Flatten the targets to be compatible with the flattened logits
        targets_flat = tf.reshape(self.targets, (-1, ))
        # Get the loss over all outputs
        loss = tf.nn.sparse_softmax_cross_entropy_with_logits(
            self.logits_flat, targets_flat, name='x_entropy')
        self.loss = tf.reduce_mean(loss)
        trainable_variables = tf.trainable_variables()
        gradients = tf.gradients(loss, trainable_variables)
        gradients, _ = tf.clip_by_global_norm(
            gradients, 5)
        self.train_op = optimizer.apply_gradients(
            zip(gradients, trainable_variables))

    def sample(self, session, prime_string, sample_length):
        self.reset_state(session)
        # Prime state
        print('prime_string: ', prime_string)
        for character in prime_string:
            character_idx = self.label_map[character]
            out = session.run(
                self.probabilities,
                feed_dict={self.inputs: np.asarray([[character_idx]])})
            sample_label = np.random.choice(
                self.labels, size=(1),  p=out[0, 0])
        output_sample = prime_string
        print('start sampling')
        # Sample for sample_length steps
        for _ in range(sample_length):
            sample_label = np.random.choice(
                self.labels, size=(1),  p=out[0, 0])[0]
            output_sample += sample_label
            sample_idx = self.label_map[sample_label]
            out = session.run(
                self.probabilities,
                feed_dict={self.inputs: np.asarray([[sample_idx]])})
        return output_sample

    def reset_state(self, session):
        for state in tf.python.util.nest.flatten(self.state_variables):
            session.run(state.initializer)

    def save(self, sess):
        self.saver.save(sess, self.save_path)

    def restore(self, sess):
        self.saver.restore(sess, self.save_path)


def train_and_sample(minibatch_iterations, restore):
    tf.reset_default_graph()
    batch_size = 64
    lstm_sizes = [512, 512]
    batch_len = 100
    learning_rate = 2e-3

    filepath = './wap.txt'

    data_feed = data_reader.DataReader(
         filepath, batch_len, batch_size)
    labels = data_feed.char_list
    print('labels: ', labels)

    save_path = './model.tf'
    model = Model(
        batch_size, batch_len, lstm_sizes, 0.8, labels,
        save_path)
    model.init_graph()
    optimizer = tf.train.AdamOptimizer(learning_rate)
    model.init_train_op(optimizer)

    init_op = tf.initialize_all_variables()
    with tf.Session() as sess:
        sess.run(init_op)
        if restore:
            print('Restoring model')
            model.restore(sess)
        model.reset_state(sess)
        start_time = time.time()
        for i in range(minibatch_iterations):
            input_batch, target_batch = next(iter(data_feed))
            loss, _ = sess.run(
                [model.loss, model.train_op],
                feed_dict={
                    model.inputs: input_batch, model.targets: target_batch})
            if i % 50 == 0 and i != 0:
                print('i: ', i)
                duration = time.time() - start_time
                print('loss: {} ({} sec.)'.format(loss, duration))
                start_time = time.time()
            if i % 1000 == 0 and i != 0:
                model.save(sess)
            if i % 100 == 0 and i != 0:
                print('Reset initial state')
                model.reset_state(sess)
            if i % 1000 == 0 and i != 0:
                print('Reset minibatch feeder')
                data_feed.reset_indices()
        model.save(sess)

    print('\n sampling after {} iterations'.format(minibatch_iterations))
    tf.reset_default_graph()
    model = Model(
        1, None, lstm_sizes, 1.0, labels, save_path)
    model.init_graph()
    init_op = tf.initialize_all_variables()
    with tf.Session() as sess:
        sess.run(init_op)
        model.restore(sess)
        print('\nSample 1:')
        sample = model.sample(
            sess, prime_string=u'\n\nThis feeling was ', sample_length=500)
        print(u'sample: \n{}'.format(sample))
        print('\nSample 2:')
        sample = model.sample(
            sess, prime_string=u'She was born in the year ', sample_length=500)
        print(u'sample: \n{}'.format(sample))
        print('\nSample 3:')
        sample = model.sample(
            sess, prime_string=u'The meaning of this all is ',
            sample_length=500)
        print(u'sample: \n{}'.format(sample))
        print('\nSample 4:')
        sample = model.sample(
            sess,
            prime_string=u'In the midst of a conversation on political matters Anna Pávlovna burst out:,',
            sample_length=500)
        print(u'sample: \n{}'.format(sample))
        print('\nSample 5:')
        sample = model.sample(
            sess, prime_string=u'\n\nCHAPTER X\n\n',
            sample_length=500)
        print(u'sample: \n{}'.format(sample))
        print('\nSample 5:')
        sample = model.sample(
            sess, prime_string=u'"If only you knew,"',
            sample_length=500)
        print(u'sample: \n{}'.format(sample))


def main():
    total_iterations = 500
    print('\n\n\nTrain for {}'.format(500))
    print('Total iters: {}'.format(total_iterations))
    train_and_sample(500, restore=False)
    for i in [500, 1000, 3000, 5000, 10000, 30000, 50000, 100000, 300000]:
        total_iterations += i
        print('\n\n\nTrain for {}'.format(i))
        print('Total iters: {}'.format(total_iterations))
        train_and_sample(i, restore=True)


if __name__ == "__main__":
    main()
