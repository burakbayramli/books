import inspect
import sys; sys.path.append('../..')

import numpy as np
from systems.basesystem import System
from systems.forecast_combine import ForecastCombineEstimated
from sysdata.csvdata import csvFuturesData
from systems.futures.rawdata import FuturesRawData
from systems.forecasting import Rules
from sysdata.configdata import Config
from systems.forecast_scale_cap import ForecastScaleCapFixed, ForecastScaleCapEstimated
from systems.account import Account

data = csvFuturesData("sysdata.tests")
rawdata = FuturesRawData()
rules = Rules()
config = Config("examples.test14.yaml")
fcs = ForecastScaleCapEstimated()
accounts=Account()

from systems.portfolio import PortfoliosEstimated
from systems.positionsizing import PositionSizing
system=System([accounts, rawdata, rules, fcs, ForecastCombineEstimated(), PositionSizing(), PortfoliosEstimated()], data, config)
print (system.portfolio.get_instrument_correlation_matrix().corr_list)

#array([[ 1.        ,  0.87041785],
#       [ 0.87041785,  1.        ]])
