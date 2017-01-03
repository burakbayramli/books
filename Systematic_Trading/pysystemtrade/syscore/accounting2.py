import inspect
import inspect
import inspect
import pandas as pd, random, numpy as np
from copy import copy, deepcopy
from pandas.tseries.offsets import BDay
import scipy.stats 
from syscore.algos import robust_vol_calc
from syscore.pdutils import  drawdown
from syscore.dateutils import BUSINESS_DAYS_IN_YEAR, ROOT_BDAYS_INYEAR, WEEKS_IN_YEAR, ROOT_WEEKS_IN_YEAR
from syscore.dateutils import MONTHS_IN_YEAR, ROOT_MONTHS_IN_YEAR
import scipy.stats as stats

DEFAULT_CAPITAL = 10000000.0
DEFAULT_ANN_RISK_TARGET = 0.16
DEFAULT_DAILY_CAPITAL=DEFAULT_CAPITAL * DEFAULT_ANN_RISK_TARGET / ROOT_BDAYS_INYEAR
    

def skew(price, forecast): 
    base_capital = DEFAULT_CAPITAL
    daily_risk_capital = DEFAULT_CAPITAL * DEFAULT_ANN_RISK_TARGET / ROOT_BDAYS_INYEAR
    use_fx = pd.Series([1.0] * len(price.index),index=price.index)
    get_daily_returns_volatility = robust_vol_calc(price.diff())
    multiplier = daily_risk_capital * 1.0 * 1.0 / 10.0
    denominator = get_daily_returns_volatility* use_fx
    numerator = forecast *  multiplier
    positions = numerator.ffill() /  denominator.ffill()
    cum_trades = positions.shift(1).ffill()
    trades_to_use=cum_trades.diff()        
    price_returns = price.diff()
    instr_ccy_returns = cum_trades.shift(1)* price_returns 
    instr_ccy_returns=instr_ccy_returns.cumsum().ffill().reindex(price.index).diff()
    pct = 100.0 * instr_ccy_returns / base_capital
    return scipy.stats.skew(pct[pd.isnull(pct) == False])
