import pandas as pd, bdm

data = pd.read_csv('chinadata.csv')

bdm.simulate(data)

