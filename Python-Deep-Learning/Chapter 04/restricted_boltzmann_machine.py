import tensorflow as tf
from tensorflow.examples.tutorials.mnist import input_data

VISIBLE_NODES = 784
HIDDEN_NODES = 400
LEARNING_RATE = 0.01

mnist = input_data.read_data_sets("MNIST_data/")

input_placeholder = tf.placeholder("float", shape=(None, VISIBLE_NODES))

weights = tf.Variable(tf.random_normal((VISIBLE_NODES, HIDDEN_NODES), mean=0.0, stddev=1. / VISIBLE_NODES))
hidden_bias = tf.Variable(tf.zeros([HIDDEN_NODES]))
visible_bias = tf.Variable(tf.zeros([VISIBLE_NODES]))

hidden_activation = tf.nn.sigmoid(tf.matmul(input_placeholder, weights) + hidden_bias)
visible_reconstruction = tf.nn.sigmoid(tf.matmul(hidden_activation, tf.transpose(weights)) + visible_bias)

final_hidden_activation = tf.nn.sigmoid(tf.matmul(visible_reconstruction, weights) + hidden_bias)

positive_phase = tf.matmul(tf.transpose(input_placeholder), hidden_activation)
negative_phase = tf.matmul(tf.transpose(visible_reconstruction), final_hidden_activation)

weight_update = weights.assign_add(LEARNING_RATE * (positive_phase - negative_phase))
visible_bias_update = visible_bias.assign_add(LEARNING_RATE *
                                              tf.reduce_mean(input_placeholder - visible_reconstruction, 0))
hidden_bias_update = hidden_bias.assign_add(LEARNING_RATE *
                                            tf.reduce_mean(hidden_activation - final_hidden_activation, 0))

train_op = tf.group(weight_update, visible_bias_update, hidden_bias_update)

loss_op = tf.reduce_sum(tf.square(input_placeholder - visible_reconstruction))

session = tf.Session()

session.run(tf.initialize_all_variables())

current_epochs = 0

for i in range(20):
    total_loss = 0
    while mnist.train.epochs_completed == current_epochs:
        batch_inputs, batch_labels = mnist.train.next_batch(100)
        _, reconstruction_loss = session.run([train_op, loss_op], feed_dict={input_placeholder: batch_inputs})
        total_loss += reconstruction_loss

    print("epochs %s loss %s" % (current_epochs, reconstruction_loss))
    current_epochs = mnist.train.epochs_completed
