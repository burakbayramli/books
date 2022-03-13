import copy
import pickle
import pandas as pd
import ipyparallel

from negotiation_model import *
from bdm_agent import *

# Defining the model objects
class BDMActor(NegotiationActor):
    DecisionClass = BDM_Agent

class NegotiationModel_(NegotiationModel):
    # Variables for median caching
    median_computed_last = -1
    median = -1
    
    def find_median(self):
        if self.median_computed_last != self.schedule.steps:
            self.median = super().find_median()
            self.median_computed_last = self.schedule.steps
        return self.median

class Model_Output:
    def __init__(self, model):
        '''
        Store data from model run.
        '''
        self.agent_vars = model.datacollector.get_agent_vars_dataframe()
        self.model_vars = model.datacollector.get_model_vars_dataframe()
        self.log = model.log

def run_model(model, n):
    new_model = copy.deepcopy(model)
    for _ in range(n):
        for agent in new_model.agents:
            agent.salience = random.random()
        new_model.step()
    return new_model


# Load data 
book_data = pd.read_csv("BDM_ColdWar.csv")
book_data.Position = (book_data.Position + 100)/200

agents = []
for i, row in book_data.iterrows():
    new_agent = BDMActor(row.Country, row.Capability, row.Position, 1)
    new_agent.decision_model.Q = 0.5
    new_agent.decision_model.T = 0.5
    agents.append(new_agent)
model = NegotiationModel_(agents)

clients = ipyparallel.Client()
print(clients.ids)
dview = clients[:]

with dview.sync_imports():
    import copy
    import random

all_models = dview.map_sync(run_model, [model]*10, [25]*10)
all_model_out = [Model_Output(m) for m in all_models]
with open("ColdWar_Experiment2_1.pickle", "wb") as f:
    pickle.dump(all_model_out, f)
print("Done!")
