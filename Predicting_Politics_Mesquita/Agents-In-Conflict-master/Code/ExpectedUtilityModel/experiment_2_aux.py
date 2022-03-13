from negotiation_model import *
from bdm_agent import *
from balancing_agent import Balancing_Agent

class BDMActor(NegotiationActor):
    DecisionClass = BDM_Agent

class BalancingActor(NegotiationActor):
    DecisionClass = Balancing_Agent

class NegotiationModel2(NegotiationModel):
    # Variables for median caching
    median_computed_last = -1
    median = -1
    
    def find_median(self):
        if self.median_computed_last != self.schedule.steps:
            self.median = super().find_median()
            self.median_computed_last = self.schedule.steps
        return self.median
