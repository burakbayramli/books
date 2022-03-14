'''
Case-Based Reasoning Agent
========================================================================

Case-based reinforcement learning for extensive-form games.

Works as follows: experience sets are stored at certain coordinates, where the
dimensions are defined at instantiation. In a new interaction, the agent
finds the closest previous experience set, and makes decisions based on that.
At the end of the interaction, the agent updates the previous experience set,
and creates a new one at the coordinates for that interaction. 

Qualitatively, you can think about it like this. An agent makes decisions based
on their experience with the most similar interaction they've had; however, the 
new interaction also influences their perception of that previous interaction.

For example: US policymakers may reason about the Iraq War based on analogies
to Vietnam; however, the lessons learned from Iraq will also color their 
perceptions of the Vietnam War.
'''

from collections import defaultdict
import random

from consul.agent_types.rl_agent import RLAgent

def get_float_dict():
    return defaultdict(float)

class CaseBased_RLAgent(RLAgent):

    #experience_library = defaultdict(lambda: defaultdict(float))
    current_coords = None
    current_case = None

    def __init__(self, learning_rate, discount_factor, beta=1, *args, **kwargs):
        '''
        Initialize a new learner.
        '''
        super().__init__(learning_rate, discount_factor, beta, *args, **kwargs)
        #self.experience_library = defaultdict(lambda: defaultdict(float))
        self.experience_library = defaultdict(get_float_dict)
        self.current_coords = None
        self.current_case = None
        self.experience = None

    def set_new_case(self, coords):
        '''
        Find the current working case and set it as the current experience.
        '''
        self.current_coords = coords
        if len(self.experience_library)>0:
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

    def _get_nearest_neighbor(self, coords):
        '''
        Find the nearest neighbor to the coordinates.
        '''
        return min(self.experience_library, 
                               key=lambda x: self.find_distance(coords, x))

    @staticmethod
    def find_distance(case1, case2):
        '''
        Find the Euclidean distance between two sets of coordinates.
        '''
        if len(case1) != len(case2):
            raise Exception("Coordinate lengths do not match!")

        total = 0
        for i in range(len(case1)):
            total += (case1[i]-case2[i])**2.0
        return (total ** 0.5)


