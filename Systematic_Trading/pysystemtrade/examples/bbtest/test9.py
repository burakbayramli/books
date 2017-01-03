import inspect
import sys; sys.path.append('../..')

from systems.provided.futures_chapter15.basesystem import futures_system
from matplotlib.pyplot import show

system = futures_system()
system.accounts.portfolio().sharpe()
system.accounts.portfolio().curve().plot()
show()
