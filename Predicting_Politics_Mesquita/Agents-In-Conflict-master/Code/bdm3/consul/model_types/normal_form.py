'''
Normal-form game model. This is a one-shot game, with a payoff matrix.
'''

class NormalGameModel(object):
    '''

    '''

    def __init__(self, agents, game_matrix, utilities=None):
        '''
        Args:
            game_matrix: A dictionary mapping tuples of player actions to 
                         payoff table labels, which in turn will be associated
                         with utilities once those are computed.
        '''
        self.agents = agents
        self.running = True
        self.outcome = None
        self.agent_moves = None
        self.game_matrix = game_matrix
        if utilities:
            self.utilities = utilities
        else:
            self.utilities = {outcome: {agent.name: 0 for agent in self.agents}
                                      for outcome in game_matrix.values()}

    def run(self):
        '''
        Get each agent's action, and return the result.
        '''
        agent_moves = []
        for agent in self.agents:
            agent_move = agent.step(self)
            agent_moves.append(agent_move)
        self.agent_moves = tuple(agent_moves)
        self.outcome = self.game_matrix[self.agent_moves]
        self.running = False
        self.distribute_utilities()

    def distribute_utilities(self):
        '''
        Distribute the utilities associated with the current outcome.
        '''
        for agent in self.agents:
            utility = self.utilities[self.outcome][agent.name]
            agent.add_utility(utility)

    def find_equilibria(self):
        '''
        Find the moves and outcomes which are pure Nash equilibria.
        '''
        nash_equilibria = []
        for moves, outcome in self.game_matrix.items():
            # Check to see if any player can improve their position by switching unilaterally
            is_nash = True
            for other_moves, other_outcome in self.game_matrix.items():
                if other_moves == moves:
                    continue
                for i in (0, 1):
                    j = 1-i
                    i_name = self.agents[i].name
                    if (moves[i] != other_moves[i] and 
                        moves[j] == other_moves[j] and
                        (self.utilities[other_outcome][i_name] > 
                            self.utilities[outcome][i_name])):
                        is_nash = False
                        break
            if is_nash:
                nash_equilibria.append(moves)
        return nash_equilibria


class NormalGameAgent(object):
    '''
    '''
    name = None

    def __init__(self, name, *args, **kwargs):
        self.name = name
        super().__init__(*args, **kwargs)

    def step(self, game_state):
        '''
        Prototype for move selector.
        '''
        raise Exception("step method not implemented.")

    def add_utility(self, utility):
        raise Exception("add_utility method not implemented.")


