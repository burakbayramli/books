import inspect
import logging
import sys; sys.path.append('../..')
from matplotlib.pyplot import show, title
from systems.provided.futures_chapter15.estimatedsystem import futures_system

system=futures_system()
system.set_logging_level("on")

system.config.instrument_weight_estimate["method"]="bootstrap" ## speed things up 
system.config.instrument_weight_estimate["equalise_means"]=False
system.config.instrument_weight_estimate["monte_runs"]=200
system.config.instrument_weight_estimate["bootstrap_length"]=104

print(system.portfolio.get_instrument_correlation_matrix().corr_list[16])

