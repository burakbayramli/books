'''
Classic BDM Model
========================

This code attempts to replicate the original BDM model, without modifications.
'''

from negotiation_model import *
from bdm_agent import *

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

class BDMActor(NegotiationActor):
    DecisionClass = Real_BDM_Agent

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
