'''
BDM Decision Rule
==================

Decision rules and heuristics attempting to replicate the BDM EU Model.
'''
from negotiation_model import DecisionModel

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



