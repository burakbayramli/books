# note must import tensorflow before gym
import random
from collections import deque

import tensorflow as tf
import gym
import numpy as np
import os

resume = True
CHECKPOINT_PATH = 'deep_q_pong'
ACTIONS_COUNT = 3
SCREEN_WIDTH, SCREEN_HEIGHT = (80, 80)
FUTURE_REWARD_DISCOUNT = 0.99
OBSERVATION_STEPS = 50000.  # time steps to observe before training
EXPLORE_STEPS = 2000000.  # frames over which to anneal epsilon
INITIAL_RANDOM_ACTION_PROB = 1.0  # starting chance of an action being random
FINAL_RANDOM_ACTION_PROB = 0.05  # final chance of an action being random
MEMORY_SIZE = 100000  # number of observations to remember
MINI_BATCH_SIZE = 100  # size of mini batches
STATE_FRAMES = 2  # number of frames to store in the state
OBS_LAST_STATE_INDEX, OBS_ACTION_INDEX, OBS_REWARD_INDEX, OBS_CURRENT_STATE_INDEX, OBS_TERMINAL_INDEX = range(5)
SAVE_EVERY_X_STEPS = 10000
LEARN_RATE = 1e-6
STORE_SCORES_LEN = 1000.
verbose_logging = True


def _create_network():
    # network weights
    convolution_weights_1 = tf.Variable(tf.truncated_normal([8, 8, STATE_FRAMES, 32], stddev=0.01))
    convolution_bias_1 = tf.Variable(tf.constant(0.01, shape=[32]))

    convolution_weights_2 = tf.Variable(tf.truncated_normal([4, 4, 32, 64], stddev=0.01))
    convolution_bias_2 = tf.Variable(tf.constant(0.01, shape=[64]))

    convolution_weights_3 = tf.Variable(tf.truncated_normal([3, 3, 64, 64], stddev=0.01))
    convolution_bias_3 = tf.Variable(tf.constant(0.01, shape=[64]))

    feed_forward_weights_1 = tf.Variable(tf.truncated_normal([256, 256], stddev=0.01))
    feed_forward_bias_1 = tf.Variable(tf.constant(0.01, shape=[256]))

    feed_forward_weights_2 = tf.Variable(tf.truncated_normal([256, ACTIONS_COUNT], stddev=0.01))
    feed_forward_bias_2 = tf.Variable(tf.constant(0.01, shape=[ACTIONS_COUNT]))

    input_layer = tf.placeholder("float", [None, SCREEN_WIDTH, SCREEN_HEIGHT,
                                           STATE_FRAMES])

    hidden_convolutional_layer_1 = tf.nn.relu(
        tf.nn.conv2d(input_layer, convolution_weights_1, strides=[1, 4, 4, 1], padding="SAME") + convolution_bias_1)

    hidden_max_pooling_layer_1 = tf.nn.max_pool(hidden_convolutional_layer_1, ksize=[1, 2, 2, 1],
                                                strides=[1, 2, 2, 1], padding="SAME")

    hidden_convolutional_layer_2 = tf.nn.relu(
        tf.nn.conv2d(hidden_max_pooling_layer_1, convolution_weights_2, strides=[1, 2, 2, 1],
                     padding="SAME") + convolution_bias_2)

    hidden_max_pooling_layer_2 = tf.nn.max_pool(hidden_convolutional_layer_2, ksize=[1, 2, 2, 1],
                                                strides=[1, 2, 2, 1], padding="SAME")

    hidden_convolutional_layer_3 = tf.nn.relu(
        tf.nn.conv2d(hidden_max_pooling_layer_2, convolution_weights_3,
                     strides=[1, 1, 1, 1], padding="SAME") + convolution_bias_3)

    hidden_max_pooling_layer_3 = tf.nn.max_pool(hidden_convolutional_layer_3, ksize=[1, 2, 2, 1],
                                                strides=[1, 2, 2, 1], padding="SAME")

    hidden_convolutional_layer_3_flat = tf.reshape(hidden_max_pooling_layer_3, [-1, 256])

    final_hidden_activations = tf.nn.relu(
        tf.matmul(hidden_convolutional_layer_3_flat, feed_forward_weights_1) + feed_forward_bias_1)

    output_layer = tf.matmul(final_hidden_activations, feed_forward_weights_2) + feed_forward_bias_2

    return input_layer, output_layer


_session = tf.Session()
_input_layer, _output_layer = _create_network()

_action = tf.placeholder("float", [None, ACTIONS_COUNT])
_target = tf.placeholder("float", [None])

readout_action = tf.reduce_sum(tf.mul(_output_layer, _action), reduction_indices=1)

cost = tf.reduce_mean(tf.square(_target - readout_action))
_train_operation = tf.train.AdamOptimizer(LEARN_RATE).minimize(cost)

_observations = deque()
_last_scores = deque()

# set the first action to do nothing
_last_action = np.zeros(ACTIONS_COUNT)
_last_action[1] = 1

_last_state = None
_probability_of_random_action = INITIAL_RANDOM_ACTION_PROB
_time = 0

_session.run(tf.initialize_all_variables())

saver = tf.train.Saver()

if not os.path.exists(CHECKPOINT_PATH):
    os.mkdir(CHECKPOINT_PATH)

if resume:
    checkpoint = tf.train.get_checkpoint_state(CHECKPOINT_PATH)
    if checkpoint:
        saver.restore(_session, checkpoint.model_checkpoint_path)


def _choose_next_action():
    new_action = np.zeros([ACTIONS_COUNT])

    if random.random() <= _probability_of_random_action:
        # choose an action randomly
        action_index = random.randrange(ACTIONS_COUNT)
    else:
        # choose an action given our last state
        readout_t = _session.run(_output_layer, feed_dict={_input_layer: [_last_state]})[0]
        if verbose_logging:
            print("Action Q-Values are %s" % readout_t)
        action_index = np.argmax(readout_t)

    new_action[action_index] = 1
    return new_action


def pre_process(screen_image):
    """ change the 210x160x3 uint8 frame into 6400 (80x80) float """
    screen_image = screen_image[35:195]  # crop
    screen_image = screen_image[::2, ::2, 0]  # downsample by factor of 2
    screen_image[screen_image == 144] = 0  # erase background (background type 1)
    screen_image[screen_image == 109] = 0  # erase background (background type 2)
    screen_image[screen_image != 0] = 1  # everything else (paddles, ball) just set to 1
    return screen_image.astype(np.float)


def _key_presses_from_action(action_set):
    # 1 = still
    # 2 = up
    # 3 = down

    if action_set[0] == 1:
        return 1
    elif action_set[1] == 1:
        return 2
    elif action_set[2] == 1:
        return 3
    raise Exception("Unexpected action")


def _train():
    # sample a mini_batch to train on
    mini_batch = random.sample(_observations, MINI_BATCH_SIZE)
    # get the batch variables
    previous_states = [d[OBS_LAST_STATE_INDEX] for d in mini_batch]
    actions = [d[OBS_ACTION_INDEX] for d in mini_batch]
    rewards = [d[OBS_REWARD_INDEX] for d in mini_batch]
    current_states = [d[OBS_CURRENT_STATE_INDEX] for d in mini_batch]
    agents_expected_reward = []
    # this gives us the agents expected reward for each action we might take
    agents_reward_per_action = _session.run(_output_layer, feed_dict={_input_layer: current_states})
    for i in range(len(mini_batch)):
        if mini_batch[i][OBS_TERMINAL_INDEX]:
            # this was a terminal frame so there is no future reward...
            agents_expected_reward.append(rewards[i])
        else:
            agents_expected_reward.append(
                rewards[i] + FUTURE_REWARD_DISCOUNT * np.max(agents_reward_per_action[i]))

    # learn that these actions in these states lead to this reward
    _session.run(_train_operation, feed_dict={
        _input_layer: previous_states,
        _action: actions,
        _target: agents_expected_reward})

    # save checkpoints for later
    if _time % SAVE_EVERY_X_STEPS == 0:
        saver.save(_session, CHECKPOINT_PATH + '/network', global_step=_time)

env = gym.make("Pong-v0")
observation = env.reset()
next_action = 1

while True:
    env.render()

    observation, reward, done, info = env.step(next_action)

    if done:
        env.reset()

    terminal = False

    screen_binary = pre_process(observation)

    if reward != 0.0:
        terminal = True
        _last_scores.append(reward)
        if len(_last_scores) > STORE_SCORES_LEN:
            _last_scores.popleft()

    # first frame must be handled differently
    if _last_state is None:
        # the _last_state will contain the image data from the last self.STATE_FRAMES frames
        _last_state = np.stack(tuple(screen_binary for _ in range(STATE_FRAMES)), axis=2)
        next_action = _key_presses_from_action(_last_action)
    else:
        screen_binary = np.reshape(screen_binary,
                                   (SCREEN_WIDTH, SCREEN_HEIGHT, 1))
        current_state = np.append(_last_state[:, :, 1:], screen_binary, axis=2)

        # store the transition in previous_observations
        _observations.append((_last_state, _last_action, reward, current_state, terminal))

        if len(_observations) > MEMORY_SIZE:
            _observations.popleft()

        # only train if done observing
        if len(_observations) > OBSERVATION_STEPS:
            _train()
            _time += 1

        # update the old values
        _last_state = current_state

        _last_action = _choose_next_action()

        # gradually reduce the probability of a random action
        if _probability_of_random_action > FINAL_RANDOM_ACTION_PROB \
                and len(_observations) > OBSERVATION_STEPS:
            _probability_of_random_action -= \
                (INITIAL_RANDOM_ACTION_PROB - FINAL_RANDOM_ACTION_PROB) / EXPLORE_STEPS

        print("Time: %s random_action_prob: %s reward %s scores differential %s" %
              (_time, _probability_of_random_action, reward,
               sum(_last_scores) / STORE_SCORES_LEN))

        next_action = _key_presses_from_action(_last_action)