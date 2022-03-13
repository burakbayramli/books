'''
Implementations of agents and auxillery functions for War and Reason Model
'''
from collections import defaultdict

from rtree import index
from consul.agent_types.spe_agent import SPESolver, SPEAgent
from consul.agent_types.rl_agent import RLAgent
from consul.agent_types.cbr_agent import CaseBased_RLAgent
from war_reason import *


# ===========================================================
#   AGENTS
# ===========================================================

class RandomAgent(WarAgent):
    def start_conflict(self, world, rival):
        pass
    def step(self, game_state):
        current_node = game_state.current_node
        possible_moves = list(game_state.tree[current_node].children.keys())
        return random.choice(possible_moves)

    def add_utility(self, utility):
        self.total_utility += utility


class RationalAgent(SPEAgent, WarAgent):
    '''
    Agent that always plays the subgame-perfect equilibrium strategy
    '''
    def add_utility(self, utility):
        self.total_utility += utility

    def start_conflict(self, world, rival):
        '''
        Reset the best-responses at the beginning of a new conflict.
        
        Args:
            world, rival: Not actually needed here, but left in for consistency.
        '''
        self.best_moves = {}
        self.node_payoffs = {}

class RLAgent(RLAgent, WarAgent):
    '''
    Strategy-free reinforcement learner
    '''
    def start_conflict(self, world, rival):
        pass

    def add_utility(self, utility):
        self.total_utility += utility
        super().add_utility(utility)

class PointDB(object):
    '''
    RTree-based database to store sets of n-dimensional points.
    Used by the case-based reasoning agents.
    '''
    def __init__(self, dim):
        self.dim = dim
        self.points = []
        p = index.Property()
        p.dimension = dim
        self.index = index.Index(properties=p)
    
    def add_point(self, point):
        if len(point) != self.dim:
            raise Exception("Incorrect dimension")
        box = tuple(list(point)*2)
        key = len(self.points)
        self.points.append(point)
        self.index.insert(key, box)
    
    def get_nearest(self, point):
        if len(point) != self.dim:
            raise Exception("Incorrect dimension")
        box = tuple(list(point)*2)
        key = list(self.index.nearest(box))
        return self.points[key[0]]


class CBRAgent(CaseBased_RLAgent, WarAgent):
    '''
    Agent that uses case-based reasoning 
        indexed on (my_stakes, their_stakes, p_win)
    '''

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.tree = PointDB(3)
        #self.beta=4
    
    def add_utility(self, utility):
        self.tree.add_point(self.current_coords)
        super().add_utility(utility)
        
    def _get_nearest_neighbor(self, coords):
        neighbor_index = self.tree.get_nearest(coords)
    
    def start_conflict(self, world, rival):
        row = world.current_row
        #case = [row[col] for col in row.keys() if col[:4] == 'wrTu']
        if self.name == row.ccode1:
            my_stakes = row.wrTu1v1 - row.wrTu1v2
            their_stakes = row.wrTu2v2 - row.wrTu2v1
            p_win = row.wrTp1win
        else:
            my_stakes = row.wrTu2v2 - row.wrTu2v1 
            their_stakes = row.wrTu1v1 - row.wrTu1v2
            p_win = row.wrTp2win

        case = (round(my_stakes, 0), round(their_stakes, 0), p_win*20)
        self.set_new_case(case)

class CollectiveCBRAgent(RLAgent, WarAgent):
    
    experience_library = defaultdict(lambda: defaultdict(float))
    tree = None
    
    def __init__(self, learning_rate, discount_factor, beta=1, *args, **kwargs):
        '''
        Initialize a new learner.
        '''
        super().__init__(learning_rate, discount_factor, beta, *args, **kwargs)
        beta=4
    
    def set_new_case(self, coords):
        '''
        Find the current working case and set it as the current experience.
        '''
        self.current_coords = coords
        if self.tree:
            self.current_case = self._get_nearest_neighbor(coords)
        else:
            self.current_case = coords
        self.experience = self.experience_library[self.current_case]
    
    def add_utility(self, utility):
        '''
        Update the experience, then duplicate it in the current spot.
        '''
        super().add_utility(utility)
        new_experience = self.experience.copy()
        self.experience_library[self.current_coords] = new_experience
        self.experience = None
        self.rebuild_tree()
        
    @classmethod
    def rebuild_tree(cls):
        cls.tree = KDTree(list(cls.experience_library.keys()))
        
    @classmethod
    def _get_nearest_neighbor(cls, coords):
        neighbor_index = cls.tree.query(coords, k=1, return_distance=False)[0,0]
        return list(cls.experience_library.keys())[neighbor_index]

    
    def start_conflict(self, world, rival):
        row = world.current_row
        #case = [row[col] for col in row.keys() if col[:4] == 'wrTu']
        if self.name == row.ccode1:
            my_stakes = row.wrTu1v1 - row.wrTu1v2
            their_stakes = row.wrTu2v2 - row.wrTu2v1
            p_win = row.wrTp1win
        else:
            my_stakes = row.wrTu2v2 - row.wrTu2v1 
            their_stakes = row.wrTu1v1 - row.wrTu1v2
            p_win = row.wrTp2win

        case = (round(my_stakes, 0), round(their_stakes, 0), p_win*20)
        self.set_new_case(case)

# ===========================================================
#   RUN ASSESSORS
# ===========================================================

def assess_run(model):
    '''
    Compare a completed run to the SPE run.
    '''
    new_agents = []
    for agent in model.agents:
        new_agent = RationalAgent(name=agent.name, capabilities=-9, R=-9)
        new_agents.append(new_agent)
    new_model = WarModel(new_agents)
    for key, val in model.utilities.items():
        new_model.utilities[key] = val
    new_model.run()
    return model.log.tversky_index(new_model.log)

def assess_moves(model):
    '''
    Find the fraction of moves which are subgame-perfect.
    '''
    solver = SPESolver()
    solver.find_spe(model, current_node='1')
    current_node='1'
    correct_moves = 0
    for event in model.log.events:
        move = event.action
        if solver.best_moves[current_node] == move:
            correct_moves += 1
        current_node = model.tree[current_node][move]
    return correct_moves / len(model.log.events)

    

