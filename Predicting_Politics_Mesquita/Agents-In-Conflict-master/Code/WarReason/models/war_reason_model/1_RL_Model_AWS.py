import sys
sys.path.append("../..")
sys.path.append("../../consul")

import csv
import copy

import pandas as pd

from consul.agent_types.rl_agent import RLAgent
from consul.agent_types.cbr_agent import CaseBased_RLAgent
from war_reason import *
from war_reason_implementations import *

import ipyparallel


N_RUNS = 100

# Load and prep the data
# =======================

data = pd.read_pickle("FullData.pickle")
# The Game-Theoretic Equilibrium
equilibrium_cols = ['eqTsq', 'eqTnego', 'eqTacqa', 'eqTacqb', 'eqTcapa', 'eqTcapb', 
                'eqTwara', 'eqTwarb']

data["Equilibrium"] = ""
for col in equilibrium_cols:
    data.loc[data[col] == 1, "Equilibrium"] = col

# Cutting out missing data
data = data[data.Equilibrium!=""]

# Observed outcomes
data["Outcome"] = ""

data.loc[((data.cwhost1<2) & (data.cwhost2<2)), "Outcome"] = "StatusQuo"
data.loc[((data.cwhost1>3) & (data.cwhost2>3)), "Outcome"] = "War"
data.loc[((data.cwhost1==data.cwhost2) & (data.cwhost1>1) & 
          (data.cwhost1<4)), "Outcome"] = "Negotiation"
data.loc[((data.cwhost1>data.cwhost2) & (data.cwhost1>1) & 
          (data.cwhost1<4)), "Outcome"] = "Acquiesce_B"
data.loc[((data.cwhost1<data.cwhost2) & (data.cwhost2>1) & 
          (data.cwhost2<4)), "Outcome"] = "Acquiesce_A"
data.loc[((data.cwhost1>data.cwhost2) & (data.cwhost1>3)), "Outcome"] = "Capitulate_B"
data.loc[((data.cwhost1<data.cwhost2) & (data.cwhost2>3)), "Outcome"] = "Capitulate_A"

# OPTIONAL
## Boost all utilities by a fixed factor
for col in data.columns:
    if col[:4] == "wrTu":
        data[col] *= 8


# Set up parallel process
# =======================

def get_outcome(m):
    return m.current_node

clients = ipyparallel.Client()
print(clients.ids)
dview = clients[:]

# Import into clients
with dview.sync_imports():
    import sys

dview.execute('sys.path.append("../..")')
dview.execute('sys.path.append("../../consul")')

with dview.sync_imports():
    from war_reason_implementations import assess_moves

dview.push({
           "EUGeneWorld": EUGeneWorld,
           "RLAgent": RLAgent,
           "get_outcome": get_outcome})

# Run model
# =====================

inputs = []
for i in range(N_RUNS):
    m = EUGeneWorld(RLAgent, data, 
                    agent_args={"learning_rate": 0.1, "discount_factor": 0.9},
                    copy_data=True, shuffle_rows=True)
    inputs.append(m)

def run_model(model):
    model.run(report_every=None,
              model_assessors={"Model_Outcome": get_outcome,
                               "Move_Quality": assess_moves})
    cols_to_keep = ['year', "ccode1", "ccode2",
                 "Outcome", "Equilibrium", "Model_Outcome", "Move_Quality"]
    out_data = model.eugene_data[cols_to_keep]
    return out_data

print("Starting: {}".format(dt.datetime.now()))
runs = dview.map_sync(run_model, inputs)
print("Done: {}".format(dt.datetime.now()))

# Write output
# =================================
for i, df in enumerate(runs):
    df["Iteration"] = i
    filename = "data_out/ex1_1_{}.pickle".format(i)
    zip_filename = filename + ".tar.bz2"
    df.to_pickle(filename)
    !tar -jcvf $zip_file $filename
    !rm $filename

print("Done!")
