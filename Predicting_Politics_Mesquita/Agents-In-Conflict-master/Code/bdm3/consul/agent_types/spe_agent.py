'''
Agent behavior tied to an ExtensiveGameModel and plays the subgame-perfect
equilibrium.
'''
from consul.event_log import EventLog
import random

class SPESolver(object):
    '''
    Find the subgame-perfect equilibrium for a given extensive-form game.
    '''

    node_payoffs = {}
    best_moves = {}
    node_to_agent = {}

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def find_spe(self, model, current_node=None, player_index=0):
        '''
        Recursively identify the SPE of a given game tree.

        Args:
            model: The ExtensiveGameModel being played.
            current_node: The name of the current node.
            player_index: The index of the player playing the current node.
        '''
        next_moves = model.tree[current_node].children
        move_payoffs = {}
        for move, child in next_moves.items():
            if model.tree[child].final:
                self.node_payoffs[child] = model.utilities[child]
            else:
                next_player = (player_index + 1) % len(model.agents)
                self.find_spe(model, child, next_player)
            move_payoffs[move] = self.node_payoffs[child]

        agent_name = model.agents[player_index].name
        self.node_to_agent[current_node] = agent_name
        best_move = max(move_payoffs, 
            key=lambda x: move_payoffs[x][agent_name])
        self.best_moves[current_node] = best_move
        self.node_payoffs[current_node] = move_payoffs[best_move]

    def make_log(self, model, log=None, current_node=None, step=0):
        '''
        Generate a notional event log for all possible SPE moves.
        '''
        top_level = False
        if log is None:
            log = EventLog()
            top_level = True
        if current_node is None:
            current_node = model.tree.root_node
        if self.best_moves == {}:
            self.find_spe(model, current_node, 0)

        current_player = self.node_to_agent[current_node]
        best_move = self.best_moves[current_node]
        log.add_event(current_player, None, step, best_move)

        for child in model.tree[current_node].children.values():
            if not model.tree[child].final:
                self.make_log(model, log, child, step+1)

        if top_level:
            return log


class SPEAgent(SPESolver):
    '''
    Given an extensive-form game, play the subgame-perfect equilibrium.

    Specifically, given an ExtensiveGameModel, plays the subgame-perfect
    equilibrium.

    Like other ConSUL behaviors, it needs to be extended with another model
    to work.
    '''

    def __init__(self, *args, **kwargs):
        self.best_moves = {}
        self.node_payoffs = {}
        super().__init__(*args, **kwargs)

    def step(self, model):
        '''
        Choose the SPE next move.
        '''
        if not model.current_node in self.best_moves:
            index = model.agents.index(self)
            self.find_spe(model, model.current_node, index)
        return self.best_moves[model.current_node]

class NashEquilibriumAgent(object):
    '''
    Given a normal-form game, play the Nash Equilibrium.
    '''

    def step(self, model):
        '''
        Choose the equilibrium move from a NormalGameModel object.
        '''
        # Get my index
        for i, agent in enumerate(model.agents):
            if agent == self:
                break

        equilibria = model.find_equilibria()
        equilibrium = random.choice(equilibria)
        return equilibrium[i]

    def add_utility(self, utility):
        pass
        
