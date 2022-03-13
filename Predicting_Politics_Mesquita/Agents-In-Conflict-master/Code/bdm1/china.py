import pandas as pd
import bdm_replication, negotiation_model

data = pd.read_csv('chinadata.csv')

print (data)

data.Resources /= 100
data.Position = ((data.Position - data.Position.min()) 
                 / (data.Position.max() - data.Position.min()))
data.Salience /= 100

print (data)

agents = []
for i, row in data.iterrows():
    agent = bdm_replication.BDMActor(row.Stakeholder, row.Resources, row.Position, row.Salience)

    agent.decision_model.Q = 1
    agent.decision_model.T = 1
    agent.decision_model.verbose = False
    agents.append(agent)
    
model = negotiation_model.NegotiationModel(agents)

print (model.find_median())

for agent in model.agents:
    print(agent)
