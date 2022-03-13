'''
Actor-Offer Negotiation Model
============================

This model is based on the general framework proposed by BDM, and extended
elsewhere.



'''

from consul.event_log import EventLog

class NegotiationModel(object):
    '''
    Parent class for a general NegotiationModel.
    '''

    agents = []
    running = True
    current_step = 0
    log = EventLog()

    def __init__(self, agents):
        self.agents = agents
        self.current_step = 0
        self.log = EventLog()


    def step(self):
        '''
        Advance the model by a single step.
        '''
        for agent in self.agents:
            agent.send_actions(self)
        for agent in self.agents:
            agent.receive_actions(self)
        self.current_step += 1

class NegotiationAgent(object):
    name = None
    capacity = 0
    issue_position = 0

    def __init__(self, name, capability, issue_position, salience, **kwargs):
        '''
        Create a new Negotiation Agent.
        '''
        self.name = name
        self.capability = capability
        self.issue_position = issue_position
        self.salience = salience
        super().__init__(**kwargs)

    def send_actions(self, model):
        raise Exception("send_actions not defined.")

    def receive_actions(self, model):
        raise Exception("receive_actions not defined.")

