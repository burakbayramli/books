'''
Code for an extensive-form game model.

Includes the ExtensiveFormModel and ExtensiveGameAgent classes.

ExtensiveGameModel defines the model structure: there is a game tree, some
agents who are activated one at a time in fixed order, and utilities associated
with each terminal node on the tree.

ExtensiveGameAgent defines the interface


'''

from itertools import cycle

from consul.model_types.utilities.game_tree import Tree
from consul.event_log import EventLog
from consul.agent_types.spe_agent import SPESolver


class ExtensiveGameModel(object):
    '''
    An environment provided by an extensive-form game tree.
    Assumes that the utilities will be computed externally.

    Will probably inherit from Mesa at some point.
    '''

    agents = []
    agent_cycle = []
    tree = None
    running = False
    current_node = None
    utilities = {}
    current_step = 0
    log = EventLog

    def __init__(self, agents, tree=None, tree_path=None):
        '''
        Set up an extensive-form game model.
        '''

        # Load the tree, if one is given.
        if tree is not None:
            self.tree = tree
        elif tree_path is not None:
            self.tree = Tree.load_tree(tree_path)

        # Set up player schedule
        # Assumes they are activated one at at a time.
        self.agents = agents
        self.agent_cycle =  cycle(agents)

        self.running = True
        self.current_node = self.tree.root_node
        self.current_step = 0
        self.log = EventLog()

    def step(self):
        '''
        Advance the model by one step.
        '''
        current_agent = next(self.agent_cycle)
        next_move = current_agent.step(self)
        self.log.add_event(current_agent.name, None, self.current_step, 
                           next_move)
        self.current_step += 1
        self.current_node = self.tree[self.current_node][next_move]

        if self.tree[self.current_node].final:
            self.running = False
            self.distribute_utilities()

    def run(self):
        '''
        Run the model steps until the running property changes to false.
        '''
        while self.running:
            self.step()

    def distribute_utilities(self):
        '''
        Distribute the utilities associated with the current terminal node.
        '''
        for agent in self.agents:
            utility = self.utilities[self.current_node][agent.name]
            agent.add_utility(utility)

    def assess_moves(self):
        '''
        Compute what fraction of both players' moves are subgame-perfect.
        Raises an exception if the model has not finished running.

        Returns:
            Fraction of moves which are on a subgame-perfect equilibrium path,
            given the current node.
        '''
        if self.running:
            raise Exception("Not done running yet.")
            
        solver = SPESolver()
        current_node = self.tree.root_node
        solver.find_spe(self, current_node=current_node)
        correct_moves = 0
        for event in self.log.events:
            move = event.action
            if solver.best_moves[current_node] == move:
                correct_moves += 1
            current_node = self.tree[current_node][move]
        return correct_moves / len(model.log.events)


class ExtensiveGameAgent(object):
    '''
    The interface for an ExtensiveGameAgent
    '''
    name = None
    def __init__(self, name, *args, **kwargs):
        self.name = name
        super().__init__(*args, **kwargs)

    def step(self, game_state):
        '''
        Prototype for a next-move selector.
        '''
        current_node = game_state.current_node
        possible_moves = game_state.tree[current_node].children.keys()
        raise Exception("step method not implemented!")

    def add_utility(self, utility):
        '''
        Receive utility from a completed game and do something with it.
        '''
        raise Exception("add_utility method not implemented!")
