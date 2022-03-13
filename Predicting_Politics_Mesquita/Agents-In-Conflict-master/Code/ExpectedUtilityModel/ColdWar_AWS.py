import copy
import pickle

from negotiation_model import *
from bdm_agent import *


MODEL_COUNT = 100
STEPS_PER_MODEL = 25

class Model_Output:
    def __init__(self, model):
        '''
        Store data from model run.
        '''
        self.agent_vars = model.datacollector.get_agent_vars_dataframe()
        self.model_vars = model.datacollector.get_model_vars_dataframe()
        self.log = model.log

if __name__ == "__main__":
    book_data = pd.read_csv("BDM_ColdWar.csv")
    book_data.Position = (book_data.Position + 100)/200 # Normalize

    agents = []
    for i, row in book_data.iterrows():
        new_agent = BDMActor(row.Country, row.Capability, row.Position, 1)
        new_agent.decision_model.Q = 0.5
        new_agent.decision_model.T = 0.5
        #new_agent.decision_model.T = 1
        agents.append(new_agent)

    all_models = []
    for i in range(10):
        model = Real_Negotiation_Model(copy.deepcopy(agents))
        for _ in range(25):
            for agent in model.agents:
                agent.salience = random.random()
            model.step()
        all_models.append(model)
        if i % 1 == 0:
            print(i)

    all_model_out = [Model_Output(m) for m in all_models]
    with open("ColdWar_Experiment1_2.pickle", "wb") as f:
        pickle.dump(all_model_out, f)
    print("DONE!")