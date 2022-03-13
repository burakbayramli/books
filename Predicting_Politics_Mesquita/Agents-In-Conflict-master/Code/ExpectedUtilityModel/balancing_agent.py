'''
Balancing Rule
===========================

Extends the BDM decision rule by replacing the offer acceptance rule with
behavior that attempts to 'balance' between competing offers.

Inherits from the basic BDM agent in order to use the utility calculations.
'''

from negotiation_model import DecisionModel
from bdm_agent import BDM_Agent

class Balancing_Agent(BDM_Agent):
    '''
    Decision rule for actor balancing between incoming threats.
    '''

    def initialize(self):
        if self.r == 1:
            self.find_r()
        self.conflicts = []

    def compute_attack_eu(self, source, target):
        '''
        Compute the expected utility from this actor's perspective of source
        attacking target. 
        Removes the terms which involve uncertainty about whether the target 
        will change position.
        '''
        p = self.get_prob(source, target)
        Us = self.u_success(source, target)
        Uf = self.u_failure(source, target)
        return p * Us + (1 - p) * Uf

    def evaluate_threats(self):
        '''
        Evaluate threats and attempt to balance between them.
        '''
        if not self.actor.incoming_threats:
            self.new_position = self.position
            return

        sum_eu = 0
        weighted_sum = 0
        for name in self.actor.incoming_threats:
            source = self.actor.model.agent_names[name]
            #weight = self.compute_eu(source, self)
            weight = self.compute_eu(self, source)
            #if self.compute_eu(self, source) > 0:
            if weight > 0:
                continue # Don't account for anyone you're eager to fight.
            #weight = self.get_prob(source, self)
            #weight = abs(weight)
            d = source.position - self.position
            weighted_sum += d * weight
            sum_eu += weight
        
        if sum_eu == 0:
            self.new_position = self.position
            return

        self.new_position = self.position + (weighted_sum / sum_eu)

    def execute_attack(self, target):
        '''
        Attack a target if still worthwhile.
        '''
        target = self.actor.model.agent_names[target]
        if target.position == self.position:
            return False
        return (self.compute_attack_eu(self, target) > 0)

    def lose_conflict(self, winner):
        pass
