import ipyparallel
import pickle
import pandas as pd
STEPS = 24
N = 15

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
    model = Real_Negotiation_Model(agents)
    
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
    from bdm_replication import BDMActor, Real_Negotiation_Model

dview.push({"agent_df": agent_df,
            "Model_Output": Model_Output})

print("Running")
model_results = dview.map_sync(run_model, [STEPS]*N)
with open("ICEWS_Ex1_1.pickle", "wb") as f:
        pickle.dump(model_results, f)
print("DONE!")

