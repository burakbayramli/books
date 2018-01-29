import collections
import numpy as np
import tensorflow as tf

from tic_tac_toe import play_game, random_player

HIDDEN_NODES = (100, 100, 100)  # number of hidden layer neurons
INPUT_NODES = 3 * 3  # board size
BATCH_SIZE = 100  # every how many games to do a parameter update?
LEARN_RATE = 1e-4
OUTPUT_NODES = INPUT_NODES
PRINT_RESULTS_EVERY_X = 1000  # every how many games to print the results

input_placeholder = tf.placeholder("float", shape=(None, INPUT_NODES))
reward_placeholder = tf.placeholder("float", shape=(None,))
actual_move_placeholder = tf.placeholder("float", shape=(None, OUTPUT_NODES))

hidden_weights_1 = tf.Variable(tf.truncated_normal((INPUT_NODES, HIDDEN_NODES[0]), stddev=1. / np.sqrt(INPUT_NODES)))
hidden_weights_2 = tf.Variable(
    tf.truncated_normal((HIDDEN_NODES[0], HIDDEN_NODES[1]), stddev=1. / np.sqrt(HIDDEN_NODES[0])))
hidden_weights_3 = tf.Variable(
    tf.truncated_normal((HIDDEN_NODES[1], HIDDEN_NODES[2]), stddev=1. / np.sqrt(HIDDEN_NODES[1])))
output_weights = tf.Variable(tf.truncated_normal((HIDDEN_NODES[-1], OUTPUT_NODES), stddev=1. / np.sqrt(OUTPUT_NODES)))

hidden_layer_1 = tf.nn.relu(
    tf.matmul(input_placeholder, hidden_weights_1) + tf.Variable(tf.constant(0.01, shape=(HIDDEN_NODES[0],))))
hidden_layer_2 = tf.nn.relu(
    tf.matmul(hidden_layer_1, hidden_weights_2) + tf.Variable(tf.constant(0.01, shape=(HIDDEN_NODES[1],))))
hidden_layer_3 = tf.nn.relu(
    tf.matmul(hidden_layer_2, hidden_weights_3) + tf.Variable(tf.constant(0.01, shape=(HIDDEN_NODES[2],))))
output_layer = tf.nn.softmax(
    tf.matmul(hidden_layer_3, output_weights) + tf.Variable(tf.constant(0.01, shape=(OUTPUT_NODES,))))

policy_gradient = tf.reduce_sum(tf.reshape(reward_placeholder, (-1, 1)) * actual_move_placeholder * output_layer)
train_step = tf.train.RMSPropOptimizer(LEARN_RATE).minimize(-policy_gradient)

sess = tf.Session()
sess.run(tf.initialize_all_variables())

board_states, actual_moves, rewards = [], [], []
episode_number = 1
results = collections.deque()


def make_move(board_state, side):
    board_state_flat = np.ravel(board_state)
    board_states.append(board_state_flat)
    probability_of_actions = sess.run(output_layer, feed_dict={input_placeholder: [board_state_flat]})[0]

    try:
        move = np.random.multinomial(1, probability_of_actions)
    except ValueError:
        # sometimes because of rounding errors we end up with probability_of_actions summing to greater than 1.
        # so need to reduce slightly to be a valid value
        move = np.random.multinomial(1, probability_of_actions / (sum(probability_of_actions) + 1e-7))

    actual_moves.append(move)

    move_index = move.argmax()
    return move_index / 3, move_index % 3


while True:
    reward = play_game(make_move, random_player)

    results.append(reward)
    if len(results) > PRINT_RESULTS_EVERY_X:
        results.popleft()

    last_game_length = len(board_states) - len(rewards)

    # we scale here so winning quickly is better winning slowly and loosing slowly better than loosing quick
    reward /= float(last_game_length)

    rewards += ([reward] * last_game_length)

    episode_number += 1

    if episode_number % BATCH_SIZE == 0:
        normalized_rewards = rewards - np.mean(rewards)
        normalized_rewards /= np.std(normalized_rewards)

        sess.run(train_step, feed_dict={input_placeholder: board_states,
                                        reward_placeholder: normalized_rewards,
                                        actual_move_placeholder: actual_moves})

        # clear batches
        del board_states[:]
        del actual_moves[:]
        del rewards[:]

    if episode_number % PRINT_RESULTS_EVERY_X == 0:
        print("episode: %s win_rate: %s" % (episode_number, 0.5 + sum(results) / (PRINT_RESULTS_EVERY_X * 2.)))
