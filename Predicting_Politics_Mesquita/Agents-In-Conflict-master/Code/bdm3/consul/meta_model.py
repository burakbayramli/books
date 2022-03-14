'''
A generic meta-model class

A meta-model, in this case (the terminology is pending) is a manager for a
population of agents which are going to be inserted in pairs (or more) into 
sub-models. 

For example, we may want to generate a population of agents, and pair them up 
at random (or based on some rules) to play an extensive-form game; however, the 
specific game may vary. (This is in fact the specific use-case this is being
developed for).

The MetaModel handles agent creation, logging, 
'''

import random

class MetaModel(object):
    '''
    MetaModel population and model manager.
    '''

    seed = None
    model_class = None
    agents = []
    agents_per_model = 2
    log = None
    steps = 0

    def __init__(self, model_class, agents_per_model=2, seed=None):
        '''
        Create a new MetaModel
        '''
        self.model_class = model_class
        self.agents_per_model = agents_per_model

        if seed:
            self.seed = seed
        else:
            self.seed = random.random()
        random.seed(self.seed)

        self.make_agents()

    def make_agents(self):
        raise Exception("make_agents not implemented.")

    def pick_agents(self, n=2):
        '''
        Randomly select n different agents and return a list.
        Override for different selection mechanics.
        '''
        selected_agents = []
        while len(selected_agents) < n:
            a = random.choice(self.agents)
            if a not in selected_agents:
                selected_agents.append(a)
        return selected_agents

    def step(self):
        '''
        Run a sub-model to completion.
        '''
        selected_agents = self.pick_agents(self.agents_per_model)
        model = self.model_class(selected_agents)
        model.run()
        self.assess_run(model)
        self.steps += 1

    def run(self, n):
        '''
        Run the meta-model for a given number of steps.
        '''
        for _ in range(n):
            self.step()

    def assess_run(self, model):
        '''
        Extract data from a completed run.
        Override for specific assessments.
        '''
        pass


