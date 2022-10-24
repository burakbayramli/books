from negotiation_model import *
from bdm_replication import *
from bdm_agent import *

import pandas as pd

data = pd.read_csv('emission.csv')

data.Resources /= 100
data.Position = ((data.Position - data.Position.min()) 
                 / (data.Position.max() - data.Position.min()))
data.Salience /= 100

agents = []
for i, row in data.iterrows():
    agent = BDMActor(row.Stakeholder, row.Resources, row.Position, row.Salience)
    agent.decision_model.Q = 1.0
    agent.decision_model.T = 0.5
    agent.decision_model.verbose = False
    agents.append(agent)

model = NegotiationModel(agents)

print (model.find_median())

for i in range(20):
    print ('step', i)
    #for agent in model.agents: print(agent)
    model.step()
    print (model.find_mean())
    
for agent in model.agents:
    print(agent)
  
