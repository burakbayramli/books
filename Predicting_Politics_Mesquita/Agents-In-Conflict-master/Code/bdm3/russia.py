from negotiation_model import *
from bdm_replication import *
from bdm_agent import *
import pandas as pd, io

s = """
Stakeholder,Resources,Position,Salience
Russia,70,100,100
Ukraine,40,40,100
USA,80,40,70
Europe,50,90,100
China,60,100,100
"""
data = pd.read_csv(io.StringIO(s))

data.Resources /= 100
data.Position = ((data.Position - data.Position.min()) 
                 / (data.Position.max() - data.Position.min()))
data.Salience /= 100

agents = []
for i, row in data.iterrows():
    agent = BDMActor(row.Stakeholder, row.Resources, row.Position, row.Salience)
    agent.decision_model.Q = 0.5
    agent.decision_model.T = 0.8
    agent.decision_model.verbose = False
    agents.append(agent)

model = NegotiationModel(agents)

print (model.find_median())
for agent in model.agents:
    print(agent)

for i in range(100):
    print ('step', i)
    model.step()
    print (model.find_median())            
  
print (model.find_median())
for agent in model.agents:
    print(agent)
