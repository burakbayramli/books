import inspect
import sys; sys.path.append('../..')
from systems.provided.example.simplesystem import simplesystem

from systems.provided.futures_chapter15.basesystem import futures_system
from matplotlib.pyplot import show

system = futures_system(log_level="on")
print(system.accounts.portfolio().sharpe())
print(system.accounts.portfolio().skew())

