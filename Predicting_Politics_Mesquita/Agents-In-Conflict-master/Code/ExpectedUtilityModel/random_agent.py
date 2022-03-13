'''
Random Agent
===============================

A variant of the BDM replication agent where decisions are made
at random from among allowed ones, as opposed to maximizing the original
heuristics (to the extent that the original model is maximizing anything). 
'''

import random
from negotiation_model import *
from bdm_replication import Real_BDM_Agent

class Random_Agent(Real_BDM_Agent):

    # Random probability parameters
    p_threat = 1 # P of threat, given eu>0
    p_react_to_offer = 1 # Prob that target will set offer_accepted
    p_follow_through = 1 # Prob that an agent will attack when it's supposed to
    randomize_r = False  # Whether to randomize r values or not.

    def find_r(self):
        '''
        Set r randomly
        '''
        if self.randomize_r:
            self.r = random.random() + random.random()
        else:
            super().find_r()


    def decide_threat(self, target):
        '''
        Decide whether to send a threat to the target.
        '''
        eu = self.compute_eu(self, target)
        if eu > 0.0001 and random.random() < self.p_threat: 
            if self.position == target.position:
                raise Exception("This shouldn't happen")
            return True
        else:
            return False

    def evaluate_threats(self):
        '''
        Evaluate threats, choose a credible one at random.
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

        if not threats:
            return

        best_offer = random.choice(threats)

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
            source = best_offer[0]
            if random.random() < self.p_react_to_offer:
                self.actor.model.agent_names[source].\
                    decision_model.offer_accepted = True
            self.log("{} compromise with {}".format(self.name, best_offer[0]))
        elif abs(my_eu) > eu:
            # Capitulation
            self.new_position = best_offer[2]
            self.log("{} capitulates to {}".format(self.name, best_offer[0]))
            source = best_offer[0]
            if random.random() < self.p_react_to_offer:
                self.actor.model.agent_names[source].\
                    decision_model.offer_accepted = True
        else:
            print(self)
            print(threats)
            raise Exception("Should never be here.")

    def execute_attack(self, target):
        if (target in self.conflicts and 
            self.name in 
                self.actor.model.agent_names[target].decision_model.conflicts
            and random.random() < self.p_follow_through):
            return True
        else:
            return False


class RandomActor(NegotiationActor):
    DecisionClass = Random_Agent