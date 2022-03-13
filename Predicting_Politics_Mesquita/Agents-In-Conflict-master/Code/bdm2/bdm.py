'''
BDM Decision Rule
==================

Decision rules and heuristics attempting to replicate the BDM EU Model.
'''

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



class BDM_Agent(DecisionModel):
    '''
    Decision rule and heuristics for a BDM actor.
    '''

    def __init__(self, actor, Q=1, T=1, verbose=False):
        super().__init__(actor)
        self.Q = Q
        self.T = T
        self.r = 1
        self.conflicts = []
        self.verbose = verbose
        self.action_log = []

    def initialize(self):
        self.find_r()
        self.conflicts = []

    def find_r(self):
        '''
        Estimate current risk acceptance.
        '''
        self.r = 1
        real_position = self.position # Cache for the ending
        security_levels = []
        for i in range(11):
            self.actor.position = i / 10
            security_levels.append(self.security_level())
        self.position = real_position
        real_security = self.security_level()
        max_security = max(security_levels)
        min_security = min(security_levels)
        R = 2 * real_security - max_security - min_security
        R /= max_security - min_security
        self.r = (1 - R/3)/(1 + R/3)
        # Temp fix
        if self.r < 0:
            self.r = 0.5
        if self.r > 2:
            self.r = 2.0

    def security_level(self):
        '''
        Compute all probabilities of winning across all challengers.
        '''
        total_p = 0
        for agent in self.actor.model.agents:
            if agent is not self.actor:
                #total_p += self.get_prob(self, agent)
                total_p += self.get_prob(agent, self)
        return total_p

    def u_success(self, source, target):
        '''
        This agent's perception of the utility to source of winning a conflict
        against target.
        '''
        return 2 - 4*(0.5 - 0.5*abs(source.position - target.position))**self.r

    def u_failure(self, source, target):
        '''
        This agent's perception of the utility to source of losing a conflict
        against target.
        '''
        return 2 - 4*(0.5 + 0.5*abs(source.position-target.position))**self.r

    def u_statusquo(self):
        '''
        This agent's perception of the status quo.
        '''
        return 2 - 4*0.5**self.r

    def u_passive_better(self, source, target):
        '''
        The agent's estimated utility of the situation improving for the source
        absent action.
        '''
        #mu = self.actor.model.find_mean()
        mu = self.actor.model.find_median()
        d = abs(source.position - mu) + abs(target.position - mu)
        return 2 - 4 * (0.5 - 0.25*d)**self.r

    def u_passive_worse(self, source, target):
        '''
        The agent's estimated utility of the situation deteriorating for the
        source absent action.
        '''
        #mu = self.actor.model.find_mean()
        mu = self.actor.model.find_median()
        d = abs(source.position - mu) + abs(target.position - mu)
        return 2 - 4 * (0.5 + 0.25*d)**self.r

    def get_prob(self, source, target):
        '''
        Get this agent's perception of the probability of source defeating
        target.
        '''
        if source.position == target.position:
            return 0
        top = 0
        bottom = 0
        for agent in self.actor.model.agents:
            d1 = abs(agent.position - source.position)#**self.r
            d2 = abs(agent.position - target.position)#**self.r
            votes = abs(d2 - d1) * agent.salience * agent.capability
            bottom += votes
            if d1 < d2:
                top += votes
        return top / bottom

    def compute_eu(self, source, target):
        '''
        Compute the expected utility from this actor's perspective of source
        threatening target.
        '''
        if source.position == target.position:
            return 0
        p = self.get_prob(source, target)
        Us = self.u_success(source, target)
        Uf = self.u_failure(source, target)
        Usq = self.u_statusquo()
        Ub = self.u_passive_better(source, target)
        Uw = self.u_passive_worse(source, target)
        s = target.salience
        eu = s * (p*Us + (1-p)*Uf) + (1-s)*Us # Action portion of EU
        eu -= self.Q * Usq + (1 - self.Q) * ( self.T * Ub + (1 - self.T) * Uw )
        return eu

    def decide_threat(self, target):
        '''
        Decide whether to send a threat to the target.
        '''
        eu = self.compute_eu(self, target)
        if eu > 0.0001: # Deal with ~0 floating point errors.
            if self.position == target.position:
                raise Exception("This shouldn't happen")
            return True
        else:
            return False

    def evaluate_threats(self):
        '''
        Evaluate threats according to the BDM rules.

        Per BDM, the player selects at most one offer, based (a) on the highest
        (perceived) incoming EU, and (b) based on the one requiring the least
        change in position. 
        '''
        self.new_position = self.position
        if not self.actor.incoming_threats:
            return
        
        # Find the offer with the highest incoming EU
        threats = []
        for name in self.actor.incoming_threats:
            source = self.actor.model.agent_names[name]
            eu = self.compute_eu(source, self)
            threats.append((name, eu, source.position))
        max_eu = max(threats, key=lambda x: x[1])[1]
        best_offers = [threat for threat in threats if threat[1]==max_eu]
        best_offer = min(best_offers, key=lambda x: abs(self.position -x[2]))

        # Figure out what kind of offer it is:
        eu = best_offer[1] # Their EU
        my_eu = self.compute_eu(self, source)
        if my_eu > 0:
            self.log("{} conflict with {}".format(self.name, best_offer[0]))
            self.conflicts.append(best_offer[0])
        elif abs(my_eu) < eu:
            # Compromise:
            delta = (best_offer[2] - self.position) * abs(my_eu / eu)
            self.new_position = self.position + delta
            self.log("{} compromise with {}".format(self.name, best_offer[0]))
        elif abs(my_eu) > eu:
            # Capitulation
            self.new_position = best_offer[2]
            self.log("{} capitulates to {}".format(self.name, best_offer[0]))
        else:
            print(self)
            print(threats)
            raise Exception("Should never be here.")

    def execute_attack(self, target):
        if (target in self.conflicts and 
            self.name in self.actor.model.agent_names[target].decision_model.conflicts):
            return True
        else:
            return False

    def allocate_support(self, side_1, side_2):
        '''
        Allocate support between the two sides.
        '''
        d1 = abs(self.position - side_1.position)#**self.r
        d2 = abs(self.position - side_2.position)#**self.r
        return (d2 - d1) * self.salience * self.capability

    def win_conflict(self, loser):
        pass

    def lose_conflict(self, winner):
        pass

    def finalize(self):
        self.actor.position = self.new_position

    def log(self, message):
        '''
        Output some message, or (TODO) append to internal log.
        '''
        if self.verbose:
            print(message)
        self.action_log.append(message)


'''
Classic BDM Model
========================

This code attempts to replicate the original BDM model, without modifications.
'''

class Real_BDM_Agent(BDM_Agent):
    
    offer_accepted = False
    
    def security_level(self, target):
        total_eu = 0
        for agent in self.actor.model.agents:
            if agent is not target:
                total_eu += self.compute_eu(agent, target)
        return total_eu
    
    def find_r(self):
        '''
        Estimate current risk acceptance.
        '''
        self.r = 1
        security_levels = []
        for agent in self.actor.model.agents:
            security_levels.append(self.security_level(agent))
        real_security = self.security_level(self)
        max_security = max(security_levels)
        min_security = min(security_levels)
        R = 2 * real_security - max_security - min_security
        R /= max_security - min_security
        self.r = (1 - R/3)/(1 + R/3)
    
    def lose_conflict(self, winner):
        self.new_position = winner.position
    
    '''
    def u_statusquo(self):
        mu = self.actor.model.find_median()
        return 1 - 2 * abs(self.position - mu)
        #return 2 - 4*(0.5 + 0.5*abs(self.position - mu))**self.r
    '''
    
    def evaluate_threats(self):
        '''
        Evaluate threats according to the BDM rules.

        Per BDM, the player selects at most one offer, based (a) on the highest
        (perceived) incoming EU, and (b) based on the one requiring the least
        change in position. 
        '''
        self.new_position = self.position
        if not self.actor.incoming_threats:
            return
        
        # Find the offer with the highest incoming EU
        threats = []
        for name in self.actor.incoming_threats:
            source = self.actor.model.agent_names[name]
            eu = self.compute_eu(source, self)
            # Start modified.
            if eu < 0:
                continue # Not credible
            my_eu = self.compute_eu(self, source)
            if my_eu > 0:
                p = self.get_prob(self, source)
                pos = p * self.position + (1 - p) * source.position
            elif abs(my_eu) <= eu:
                # Compromise:
                delta = (source.position - self.position) * abs(my_eu / eu)
                pos = self.position + delta
            elif abs(my_eu) > eu:
                # Capitulate
                pos = source.position
            else:
                raise Exception("Uh oh")
            threats.append((name, eu, pos))

            #threats.append((name, eu, source.position))
        #max_eu = max(threats, key=lambda x: x[1])[1]
        #best_offers = [threat for threat in threats if threat[1]==max_eu]
        #best_offer = min(best_offers, key=lambda x: abs(self.position - x[2]))
        if not threats:
            return
        min_move = min(threats, key=lambda x: abs(self.position - x[2]))[2]
        best_offers = [threat for threat in threats if threat[2]==min_move]
        best_offer = max(best_offers, key=lambda x: x[1])


        # Figure out what kind of offer it is:
        eu = best_offer[1] # Their EU
        my_eu = self.compute_eu(self, source)
        if my_eu > 0:
            self.log("{} conflict with {}".format(self.name, best_offer[0]))
            self.conflicts.append(best_offer[0])
        elif abs(my_eu) < eu:
            # Compromise:
            #delta = (best_offer[2] - self.position) * abs(my_eu / eu)
            #self.new_position = self.position + delta
            self.new_position = best_offer[2]
            source = best_offer[0]
            self.actor.model.agent_names[source].decision_model.offer_accepted = True
            self.log("{} compromise with {}".format(self.name, best_offer[0]))
        elif abs(my_eu) > eu:
            # Capitulation
            self.new_position = best_offer[2]
            self.log("{} capitulates to {}".format(self.name, best_offer[0]))
            source = best_offer[0]
            self.actor.model.agent_names[source].decision_model.offer_accepted = True
        else:
            print(self)
            print(threats)
            raise Exception("Should never be here.")
    
    def finalize(self):
        if self.offer_accepted:
            self.new_position = self.position
            self.conflicts = []
        if self.verbose:
            p0 = self.position
            p1 = self.new_position
            print("{}: {:.2f} -> {:.2f}".format(self.name, p0, p1))
        super().finalize()
        self.offer_accepted = False

class NegotiationActor(object):
    '''
    Outline for a negotiaton actor.
    '''

    DecisionClass = None

    def __init__(self, name, capability, position, salience):
        '''
        New Negotiation Agent
        '''
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

        
class BDMActor(NegotiationActor):
    DecisionClass = Real_BDM_Agent

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
            self.schedule.add(agent)

        self.agent_names = {agent.name: agent for agent in agents}

        self.running = True
        self.stage_delta = 1 / len(self.step_stages)
        
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



    
class Real_Negotiation_Model(NegotiationModel):
    step_stages = ["initialize", "send_threats", "resolve_threats", 
                    "resolve_attacks", ("resolve_attacks", "Model"), 
                    "finalize"]

    # Variables for median caching
    median_computed_last = -1
    median = -1

    def resolve_attacks(self):

        # Resolve attacks
        done_attacks = []
        new_attacks = self.log.get_events(timestamp=self.schedule.steps,
            action="Attack")
        for attack in new_attacks:
            
            source = self.agent_names[attack.source]
            target = self.agent_names[attack.target]
            if (source.decision_model.offer_accepted or 
                target.decision_model.offer_accepted):
                self.log.events.remove(attack) # This attack didn't happen.
                continue
            if (attack.target, attack.source) in done_attacks:
                continue
            if self.resolve_conflict(source, target):
                winner, loser = source, target
            else:
                winner, loser = target, source
            winner.win_conflict(loser)
            loser.lose_conflict(winner)
            done_attacks.append((attack.source, attack.target))

    def find_median(self):
        if self.median_computed_last != self.schedule.steps:
            self.median = super().find_median()
            self.median_computed_last = self.schedule.steps
        return self.median

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

from mesa.datacollection import DataCollector

def get_median(m):
    return m.find_median()

def get_mean(m):
    return m.find_mean()

def get_position(a):
    return a.position



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
    
def simulate(data):
    data.Resources /= 100
    data.Position = ((data.Position - data.Position.min()) 
                     / (data.Position.max() - data.Position.min()))
    data.Salience /= 100

    agents = []
    for i, row in data.iterrows():
        agent = BDMActor(row.Stakeholder, row.Resources, row.Position, row.Salience)
        agent.decision_model.Q = 1
        agent.decision_model.T = 1
        agent.decision_model.verbose = False
        agents.append(agent)

    model = NegotiationModel(agents)

    print (model.find_median())

    for agent in model.agents:
        print(agent)
            
