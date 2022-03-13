import ipyparallel
import pickle
import pandas as pd

from negotiation_model import *
from bdm_agent import *
from experiment_2_aux import *

STEPS = 24
N = 100

class Model_Output:
    def __init__(self, model):
        '''
        Store data from model run.
        '''
        self.agent_vars = model.datacollector.get_agent_vars_dataframe()
        self.model_vars = model.datacollector.get_model_vars_dataframe()
        self.log = model.log


def run_model(n):
    '''
    Assemble a model from the agent_df data, and return model results.
    '''
    # Assemble model
    agents = []
    for i, row in agent_df.iterrows():
        name = row.Name
        capability = row.Capability
        position = row.Position
        salience = random.random()
        if not position.is_integer():
            position = 0.5 + random.normalvariate(0, 0.1)
        new_agent = BDMActor(name, capability, position, salience)

        new_agent.decision_model.Q = 0.5
        new_agent.decision_model.T = 0.5
        agents.append(new_agent)
    model = NegotiationModel2(agents)
    
    # Run Model
    for j in range(n):
        model.step()
    return Model_Output(model)

# Load data
agent_df = pd.read_csv("ICEWS_2004_agents.csv")

# Prep cluster
clients = ipyparallel.Client()
dview = clients[:]

with dview.sync_imports():
    import random
    #from negotiation_model import NegotiationModel
    #from bdm_agent import BDM_Agent
    

dview.push({"agent_df": agent_df,
            "Model_Output": Model_Output,
            "BDMActor": BDMActor,
            "NegotiationModel2": NegotiationModel2 })

print("Running")
model_results = dview.map_sync(run_model, [STEPS]*N)
with open("ICEWS_Ex3_1.pickle", "wb") as f:
    pickle.dump(model_results, f)
print("DONE!")

