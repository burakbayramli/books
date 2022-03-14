'''
BDM-Based Negotiation Model
===========================

This model is based on the general framework proposed by BDM, and extended
elsewhere. The decision-free version of the model goes as follows:

Agents are characterized by cability, a position (on some issue; initially one
-dimensional), and the salience of that issue.

Each step of the model has the following sub-phases:
    0. Update risk acceptances?
    1. Agents send threats ('offers' in BDM nomenclature) to one another.
    2. Agents evaluate incoming threats, change their position.
    3. Agents with open threats choose whether to follow up on them. Conflicts
       ensue.
'''
from itertools import permutations
import random

from consul.event_log import EventLog
from staged_schedule import StagedActivation
from mesa.datacollection import DataCollector


def get_median(m):
    return m.find_median()

def get_mean(m):
    return m.find_mean()

def get_position(a):
    return a.position

class NegotiationModel(object):
    '''
    '''

    step_stages = ["initialize", "send_threats", "resolve_threats", 
                    "resolve_attacks", "finalize", 
                    ("resolve_attacks", "Model")]


    def __init__(self, agents):
        '''
        Instantiate a new model.
        '''

        self.agents = agents
        self.schedule = StagedActivation(self, self.step_stages)
        for agent in self.agents:
            agent.model = self
            self.schedule.add(agent)

        self.agent_names = {agent.name: agent for agent in agents}

        self.running = True
        self.stage_delta = 1 / len(self.step_stages)
        
        self.log = EventLog()
        self.datacollector = DataCollector(
            {"Median": get_median, #lambda m: m.find_median(),
            "Mean": get_mean}, #lambda m: m.find_mean()},
            {"Position": get_position}) #lambda a: a.position})


    def step(self):
        '''
        One step of the model, through all step_stages.
        '''
        self.datacollector.collect(self)
        self.schedule.step()
        if all(a.position == self.agents[0].position for a in self.agents):
            self.running = False

    def run_model(self, steps):
        '''
        Run model for `steps` steps, or until it converges.
        '''
        while self.schedule.steps < steps and self.running:
            self.step()
        
    def resolve_attacks(self):
        done_attacks = []
        new_attacks = self.log.get_events(timestamp=self.schedule.steps,
            action="Attack")
        for attack in new_attacks:
            if (attack.target, attack.source) in done_attacks:
                continue
            source = self.agent_names[attack.source]
            target = self.agent_names[attack.target]
            if self.resolve_conflict(source, target):
                winner, loser = source, target
            else:
                winner, loser = target, source
            winner.win_conflict(loser)
            loser.lose_conflict(winner)
            done_attacks.append((attack.source, attack.target))

    def resolve_conflict(self, side_1, side_2):
        '''
        Resolve an active conflict between sides 1 and 2.

        Args:
            side_1, side_2: Agent objects
        Returns:
            True if side_1 wins, False if side_2 wins.
        '''
        if side_1.position == side_2.position:
            print(side_1)
            print(side_2)
            raise Exception("This attack shouldn't be happening.")
        side_1_power = 0
        side_2_power = 0
        for agent in self.agents:
            v = agent.allocate_support(side_1, side_2)
            if v < 0:
                side_2_power += abs(v)
            else:
                side_1_power += v
        try:
            p = side_1_power / (side_1_power + side_2_power)
        except Exception as e:
            print(side_1)
            print(side_2)
            raise e
        if random.random() < p:
            return True
        else:
            return False

    def add_event(self, source, target, action):
        '''
        Add an event to the event log.
        '''
        if type(source) is not str:
            source = source.name
        if type(target) is not str:
            target = target.name
        self.log.add_event(source, target, self.schedule.steps, action)

    def find_median(self):
        '''
        Returns the estimated median voter position.
        Uses the original BDM model calculation.
        '''
        pairwise_contests = {}
        for i, j in permutations(self.agents, 2):
            votes = 0
            for agent in self.agents:
                delta = (abs(agent.position - j.position) 
                    - abs(agent.position - i.position))
                votes += agent.capability * agent.salience * delta
            pairwise_contests[(i.position, j.position)] = votes
        return max(pairwise_contests, key=lambda x: pairwise_contests[x])[0]

    def find_mean(self):
        '''
        Returns the estimated mean position, weighted by capability*salience.
        '''
        weighted_sum = 0
        weights = 0
        for agent in self.agents:
            weight = agent.capability * agent.salience
            weighted_sum += weight * agent.position
            weights+= weight
        return weighted_sum / weights


class NegotiationActor(object):
    '''
    Outline for a negotiaton actor.
    '''

    DecisionClass = None

    def __init__(self, name, capability, position, salience):
        '''
        New Negotiation Agent
        '''
        self.model = None
        self.name = name
        self.unique_id = self.name
        self.capability = capability
        self.position = position
        self.initial_position = position
        self.salience = salience
        self.r = 1
        self.incoming_threats = []
        self.outgoing_threats = []
        self.decision_model = self.DecisionClass(self)


    def initialize(self, model):
        '''
        Observe the current state of the model and update as needed.
        '''
        self.model = model
        self.incoming_threats = []
        self.outgoing_threats = []
        self.decision_model.initialize()


    def send_threats(self, model):
        '''
        Send threats to other agents.
        '''
        for agent in model.agents:
            if agent is self:
                continue
            if self.decision_model.decide_threat(agent):
                model.add_event(self, agent, "Threaten")
                agent.incoming_threats.append(self.name)
                self.outgoing_threats.append(agent.name)

    def resolve_threats(self, model):
        '''
        Figure out which threats to respond to.
        '''
        self.decision_model.evaluate_threats()

    def resolve_attacks(self, model):
        '''
        Decide whether to carry out any threats.
        '''

        for target in self.outgoing_threats:
            if self.decision_model.execute_attack(target):
                model.add_event(self, target, "Attack")

    def allocate_support(self, side_1, side_2):
        '''
        Allocate some capability in support of one side or the other in a
        bilateral conflict. 

        Returns positive if supporting side_1, negative if supporting side_2.
        '''

        support = self.decision_model.allocate_support(side_1, side_2)
        if abs(support) > self.capability:
            raise Exception("Support greater than capability.")
        return support

    def win_conflict(self, loser):
        self.decision_model.win_conflict(loser)

    def lose_conflict(self, winner):
        self.decision_model.lose_conflict(winner)

    def finalize(self, model):
        self.decision_model.finalize()

    def __str__(self):
        template = "{:20}\tPosition: {:.2f}\t" 
        template += "Capability: {:.2f}\tSalience: {:.2f}"
        return template.format(self.name, self.position, self.capability,
            self.salience)

    def __repr__(self):
        return self.__str__()

class DecisionModel(object):
    '''
    Generic framework for a decision model. This makes
    it easy to compose decision models with agents, clone agents
    and swap decision models (for look-ahead purposes) etc.
    '''

    def __init__(self, actor):
        '''
        Create a new Decision Model, tied to the parent actor.
        '''
        self.actor = actor

    @property
    def name(self):
        return self.actor.name

    @property
    def capability(self):
        return self.actor.capability

    @property
    def position(self):
        return self.actor.position

    @position.setter
    def position(self, value):
        self.actor.position = value

    @property
    def salience(self):
        return self.actor.salience



