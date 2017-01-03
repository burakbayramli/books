import sys; sys.path.append('../..')
import matplotlib.pyplot as plt
import pandas as pd

from systems.provided.futures_chapter15.basesystem import futures_system
system=futures_system()
#dfs = []
#for c in ['EDOLLAR','US10','EUROSTX','V2X','MXP','CORN']:
#    pandl = system.accounts.pandl_for_subsystem(c)
#    dfs.append(pandl)
#    pandl.to_csv("out-%s.csv" % c)
print(system.portfolio.get_instrument_correlation_matrix())
    
#df = pd.concat(dfs,axis=1)
#df.to_csv("out.csv")
