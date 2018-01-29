# note must import tensorflow before gym
import pickle
from collections import deque

import tensorflow as tf
import gym
import numpy as np
import matplotlib.pyplot as plt

env = gym.make('CartPole-v0')

ACTIONS_COUNT = 2
FUTURE_REWARD_DISCOUNT = 0.9
LEARN_RATE_ACTOR = 0.01
LEARN_RATE_CRITIC = 0.01
STORE_SCORES_LEN = 5
GAMES_PER_TRAINING = 3
INPUT_NODES = env.observation_space.shape[0]

ACTOR_HIDDEN = 20
CRITIC_HIDDEN = 20

session = tf.Session()

actor_feed_forward_weights_1 = tf.Variable(tf.truncated_normal([INPUT_NODES, ACTOR_HIDDEN], stddev=0.01))
actor_feed_forward_bias_1 = tf.Variable(tf.constant(0.0, shape=[ACTOR_HIDDEN]))

actor_feed_forward_weights_2 = tf.Variable(tf.truncated_normal([ACTOR_HIDDEN, ACTIONS_COUNT], stddev=0.01))
actor_feed_forward_bias_2 = tf.Variable(tf.constant(0.1, shape=[ACTIONS_COUNT]))

actor_input_placeholder = tf.placeholder("float", [None, INPUT_NODES])
actor_hidden_layer = tf.nn.tanh(
    tf.matmul(actor_input_placeholder, actor_feed_forward_weights_1) + actor_feed_forward_bias_1)
actor_output_layer = tf.nn.softmax(
    tf.matmul(actor_hidden_layer, actor_feed_forward_weights_2) + actor_feed_forward_bias_2)

actor_action_placeholder = tf.placeholder("float", [None, ACTIONS_COUNT])
actor_advantage_placeholder = tf.placeholder("float", [None, 1])

policy_gradient = tf.reduce_mean(actor_advantage_placeholder * actor_action_placeholder * tf.log(actor_output_layer))
actor_train_operation = tf.train.AdamOptimizer(LEARN_RATE_ACTOR).minimize(-policy_gradient)

critic_feed_forward_weights_1 = tf.Variable(tf.truncated_normal([INPUT_NODES, CRITIC_HIDDEN], stddev=0.01))
critic_feed_forward_bias_1 = tf.Variable(tf.constant(0.0, shape=[CRITIC_HIDDEN]))

critic_feed_forward_weights_2 = tf.Variable(tf.truncated_normal([CRITIC_HIDDEN, 1], stddev=0.01))
critic_feed_forward_bias_2 = tf.Variable(tf.constant(0.0, shape=[1]))

critic_input_placeholder = tf.placeholder("float", [None, INPUT_NODES])
critic_hidden_layer = tf.nn.tanh(
    tf.matmul(critic_input_placeholder, critic_feed_forward_weights_1) + critic_feed_forward_bias_1)
critic_output_layer = tf.matmul(critic_hidden_layer, critic_feed_forward_weights_2) + critic_feed_forward_bias_2

critic_target_placeholder = tf.placeholder("float", [None, 1])

critic_cost = tf.reduce_mean(tf.square(critic_target_placeholder - critic_output_layer))
critic_train_operation = tf.train.AdamOptimizer(LEARN_RATE_CRITIC).minimize(critic_cost)

critic_baseline = critic_target_placeholder - critic_output_layer

scores = deque(maxlen=STORE_SCORES_LEN)

# set the first action to do nothing
last_action = np.zeros(ACTIONS_COUNT)
last_action[1] = 1

time = 0

session.run(tf.initialize_all_variables())


def choose_next_action(state):
    probability_of_actions = session.run(actor_output_layer, feed_dict={actor_input_placeholder: [state]})[0]
    try:
        move = np.random.multinomial(1, probability_of_actions)
    except ValueError:
        # sometimes because of rounding errors we end up with probability_of_actions summing to greater than 1.
        # so need to reduce slightly to be a valid value
        move = np.random.multinomial(1, probability_of_actions / (sum(probability_of_actions) + 1e-6))
    return move


def train(states, actions_taken, advantages):
    # learn that these actions in these states lead to this reward
    session.run(actor_train_operation, feed_dict={
        actor_input_placeholder: states,
        actor_action_placeholder: actions_taken,
        actor_advantage_placeholder: advantages})


last_state = env.reset()
total_reward = 0
current_game_observations = []
current_game_rewards = []
current_game_actions = []

episode_observation = []
episode_rewards = []
episode_actions = []
games = 0
plot_x = []
plot_y = []

critic_costs = deque(maxlen=10)


while True:
    env.render()
    last_action = choose_next_action(last_state)
    current_state, reward, terminal, info = env.step(np.argmax(last_action))
    total_reward += reward

    if terminal:
        reward = -.10
    else:
        reward = 0.1

    current_game_observations.append(last_state)
    current_game_rewards.append(reward)
    current_game_actions.append(last_action)

    if terminal:
        games += 1
        scores.append(total_reward)

        if games % STORE_SCORES_LEN == 0:
            plot_x.append(games)
            plot_y.append(np.mean(scores))

        # get temporal difference values for critic
        cumulative_reward = 0
        for i in reversed(range(len(current_game_observations))):
            cumulative_reward = current_game_rewards[i] + FUTURE_REWARD_DISCOUNT * cumulative_reward
            current_game_rewards[i] = [cumulative_reward]

        values_t = session.run(critic_output_layer, {
            critic_input_placeholder: current_game_observations})
        advantages = []

        for i in range(len(current_game_observations) - 1):
            advantages.append([current_game_rewards[i][0] + FUTURE_REWARD_DISCOUNT*values_t[i+1][0] - values_t[i][0]])

        advantages.append([current_game_rewards[-1][0]-values_t[-1][0]])

        _, cost = session.run([critic_train_operation, critic_cost], {
                    critic_input_placeholder: current_game_observations,
                    critic_target_placeholder: current_game_rewards})

        critic_costs.append(cost)

        print("Game: %s reward %s average scores %s critic cost %s" %
              (games, total_reward,
               np.mean(scores), np.mean(critic_costs)))

        episode_observation.extend(current_game_observations)
        episode_actions.extend(current_game_actions)
        episode_rewards.extend(advantages)

        total_reward = 0
        current_game_observations = []
        current_game_rewards = []
        current_game_actions = []

        if games % GAMES_PER_TRAINING == 0:
            episode_rewards = np.array(episode_rewards)
            normalized_rewards = episode_rewards - np.mean(episode_rewards)
            normalized_rewards /= np.std(normalized_rewards)

            train(episode_observation, episode_actions, normalized_rewards)

            episode_observation = []
            episode_actions = []
            episode_rewards = []

    time += 1

    # update the old values
    if terminal:
        last_state = env.reset()
    else:
        last_state = current_state