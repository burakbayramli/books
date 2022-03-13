import random
from mesa.time import BaseScheduler

class StagedActivation(BaseScheduler):
    '''
    Scheduler where every agent has multiple stages that need to be activated,
    where all agents execute one stage before moving on to the next one.
    '''

    def __init__(self, model, method_list):
        '''
        Create an empty scheduler, with a given method_list

        Args:
            model: Model object associated with the schedule
            method_list: List of strings of method names or tuples for 
                         activation stages. Strings are assumed to be agent
                         methods; tuples take the form (method_name, "target")
                         where "target" is "Model" or "Agents". "Model" methods
                         are only called once, on the model.
                         stages. Each method must take only a model object as
                         an argument.
        '''
        super().__init__(model)
        self.method_list = method_list

    def _run_stage(self, stage):
        '''
        Run a given stage, based on its type, string or tuple.
        '''
        if type(stage) is tuple:
            if stage[1] == "Model":
                getattr(self.model, stage[0])() # Run method
                return
            elif stage[1] == "Agents":
                stage = stage[1]
            else:
                raise Exception("Invalid method target.")
        # Run method on agents
        for agent in self.agents:
            getattr(agent, stage)(self.model)

    def step(self):
        '''
        Execute a step of the model, calling all of each method one at a time.
        '''
        for stage in self.method_list:
            self._run_stage(stage)
        self.steps += 1
        self.time += 1


class RandomStagedActivation(StagedActivation):
    '''
    Scheduler where every agent has multiple stages that need to be activated,
    where all agents execute one stage before moving on to the next one, in
    random order.
    '''

    def __init__(self, model, method_list, shuffle_within_step=False):
        '''
        Create an empty scheduler, with a given method_list

        Args:
            model: Model object associated with the schedule
            method_list: List of strings of method names for agent activation
                         stages. Each method must take only a model object as
                         an argument.
            shuffle_within_step: (boolean) If True, reshuffle the agent
                                 activation order after each method. Otherwise,
                                 execute all stages in the same order within
                                 each turn.
        '''
        super().__init__(model, method_list)
        self.shuffle_within_step = shuffle_within_step

    def step(self):
        '''
        Execute the steps of all agents, in random agent order.
        '''
        random.shuffle(self.agents)
        for stage in self.method_list:
            self._run_stage(stage)    
            if self.shuffle_within_step:
                random.shuffle(self.agents)
        
        self.steps += 1
        self.time += 1
