'''
Agent behavior tied to the ExtensiveGameModel (for now) which keeps track of 
moved played and learns when utility is assigned.
'''

from collections import defaultdict
import random

import numpy as np

class RLAgent(object):
    '''
    Play moves based on past experience, learning based on utility.
    '''

    # Model parameters
    experience = defaultdict(float)
    learning_rate = 0.8
    discount_factor = 0.2
    beta = 1

    past_moves = [] # Track moves during each game.

    def __init__(self, learning_rate, discount_factor, beta=1, *args, **kwargs):
        '''
        Initialize a new learner.
        Args:
            learning_rate: The weight placed on each new incoming utility
            discount_factor: Weight placed on accumulated past experience
            beta: Intensity of choice parameter
        '''
        self.learning_rate = learning_rate
        self.discount_factor = discount_factor
        self.beta = beta
        self.experience = defaultdict(float)
        self.past_moves = []
        super().__init__(*args, **kwargs)

    def step(self, model):
        '''
        Choose next move based on current node and experience.

        The agent randomly selects a next move, weighted by its past 
        experience; if in a new state, the weight defaults to 0. After a move
        is chosen, it is added to the past_moves list, which will be learned
        from at the end of the run.

        Args:
            model: The current model object
        Returns:
            The agent's chosen move.
        '''

        current_state = model.current_node
        # Build the dictionary of possible moves, and their associated weights
        choices = {}
        for move in model.tree[current_state].children:
            key = (current_state, move)
            choices[move] = self.experience[key]
        choice = self.weighted_choice(choices, self.beta)
        self.past_moves.append((current_state, choice))
        return choice

    def add_utility(self, utility):
        '''
        Update the experience table by allocating the utility to the past_moves.

        Args:
            utility: A numeric utility value to allocate.
        '''
        for move in self.past_moves:
            self.experience[move] *= self.discount_factor
            self.experience[move] += self.learning_rate * utility
        self.past_moves = []

    @staticmethod
    def weighted_choice(choices, beta=1):
        '''
        Randomly choose a dictionary key, where the values are weights.

        Given a dictionary mapping choices c1...cn and weights w1... wn, and an
        intensity of choice parameter beta, select a choice st.
            p(ci) = exp(wi) / SUMj[exp(wj)]

        Args:
            choices: A dictionary of the form  {choice: weight ...}
            beta: Intensity of choice. 
                beta=0 => Choice is always completely random
                beta->Inf => The largest weight is deterministically chosen.

        Returns:
            A chosen dictionary key.
        '''
        # Take the exponent of each value and sum:
        denom = 0
        for w in choices.values():
            denom += np.exp(beta * w)

        # Iterate until a choice is made:
        while True:
            for choice, weight in choices.items():
                numerator = np.exp(beta*weight)
                p = numerator / denom
                if random.random() < p:
                    return choice


    def train_to_outcome(self, model, target_outcome):
        '''
        Train the agent to a particular outcome.
        '''
        tree = model.tree
        current_node = target_outcome
        moves = []
        # Continue to search for the previous 
        while current_node != tree.root_node:
            for node in tree.nodes.values():
                if node.final:
                    continue
                for move, child in node.children.items():
                    if child == current_node:
                        moves.insert(0, (node.name, move))
                        current_node = node.name
                        break
        # Figure out which moves are mine
        start_index = model.agents.index(self)
        agent_count = len(model.agents)
        my_moves = moves[start_index::agent_count]
        # Set the moves and utility
        self.past_moves = my_moves
        utility = model.utilities[target_outcome][self.name]
        self.add_utility(utility)



    


