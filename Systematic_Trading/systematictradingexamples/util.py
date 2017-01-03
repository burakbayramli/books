from scipy.optimize import minimize
import numpy as np, random
import pandas as pd
CALENDAR_DAYS_IN_YEAR = 365.25
BUSINESS_DAYS_IN_YEAR = 256.0
ROOT_BDAYS_INYEAR = BUSINESS_DAYS_IN_YEAR**.5
WEEKS_IN_YEAR = CALENDAR_DAYS_IN_YEAR / 7.0
ROOT_WEEKS_IN_YEAR = WEEKS_IN_YEAR**.5
MONTHS_IN_YEAR = 12.0
ROOT_MONTHS_IN_YEAR = MONTHS_IN_YEAR**.5
ARBITRARY_START=pd.datetime(1900,1,1)
FLAG_BAD_RETURN=-99999.0

DEFAULT_CAPITAL = 1.0
DEFAULT_ANN_RISK_TARGET = 0.16

def sharpe(price, forecast):
    base_capital = DEFAULT_CAPITAL
    daily_risk_capital = DEFAULT_CAPITAL * DEFAULT_ANN_RISK_TARGET / ROOT_BDAYS_INYEAR        
    ts_capital=pd.Series([DEFAULT_CAPITAL]*len(price), index=price.index)        
    ann_risk = ts_capital * DEFAULT_ANN_RISK_TARGET
    daily_returns_volatility = robust_vol_calc(price.diff())
    multiplier = daily_risk_capital * 1.0 * 1.0 / 10.0
    numerator = forecast *  multiplier
    positions = numerator.ffill() /  daily_returns_volatility.ffill()
    cum_trades = positions.shift(1).ffill()
    price_returns = price.diff()
    instr_ccy_returns = cum_trades.shift(1)*price_returns 
    instr_ccy_returns=instr_ccy_returns.cumsum().ffill().reindex(price.index).diff()
    mean_return = instr_ccy_returns.mean() * BUSINESS_DAYS_IN_YEAR
    vol = instr_ccy_returns.std() * ROOT_BDAYS_INYEAR
    return mean_return / vol
    
def crossover(df,col,lev):
    signals = pd.DataFrame(index=df.index) 
    signals['signal'] = 0 
    short_ma = pd.rolling_mean(df[col], 40, min_periods=1) 
    long_ma = pd.rolling_mean(df[col], 100, min_periods=1) 
    signals['signal'] = np.where(short_ma > long_ma, 1, 0) 
    df['signal'] = signals['signal'].shift(1) 
    df['ret'] = df[col].pct_change() * df['signal']
    ret = df.ret.dropna()  * lev
    return ret

def bollinger(df,col,lev):
    signals = pd.DataFrame(index=df.index) 
    signals['signal'] = np.nan
    middle = pd.rolling_mean(df[col], 40, min_periods=1) 
    std = pd.rolling_std(df[col], 40, min_periods=1)
    df['middle'] = middle
    df['top'] = middle+2*std
    df['bottom'] = middle-2*std
    signals['signal'] = np.where(df[col] > middle+2*std, -1, np.nan) 
    signals['signal'] = np.where(df[col] < middle-2*std, 1, np.nan)
    signals['signal'] = signals['signal'].fillna(method='ffill')
    df['ret'] = df[col].pct_change() * signals['signal'].shift(1)
    ret = df.ret.dropna() * lev
    return ret

def vol_equaliser(mean_list, stdev_list):
    if np.all(np.isnan(stdev_list)):
        return (([np.nan]*len(mean_list), [np.nan]*len(stdev_list)))
    avg_stdev=np.nanmean(stdev_list)
    norm_factor=[asset_stdev/avg_stdev for asset_stdev in stdev_list]    
    norm_means=[mean_list[i]/norm_factor[i] for (i, notUsed) in enumerate(mean_list)]
    norm_stdev=[stdev_list[i]/norm_factor[i] for (i, notUsed) in enumerate(stdev_list)] 
    return (norm_means, norm_stdev)

def apply_with_min_periods(xcol, my_func=np.nanmean, min_periods=0):
    not_nan=sum([not np.isnan(xelement) for xelement in xcol])    
    if not_nan>=min_periods:    
        return my_func(xcol)
    else:
        return np.nan

def vol_estimator(x, using_exponent=True, min_periods=20, ew_lookback=250):
    vol=x.apply(apply_with_min_periods,axis=0,min_periods=min_periods, my_func=np.nanstd) 
    stdev_list=list(vol)    
    return stdev_list

def mean_estimator(x, using_exponent=True, min_periods=20, ew_lookback=500):
    means=x.apply(apply_with_min_periods,axis=0,min_periods=min_periods, my_func=np.nanmean)
    mean_list=list(means)    
    return mean_list

def str2Bool(x):
    if type(x) is bool:
        return x
    return x.lower() in ("t", "true")

def correlation_single_period(data_for_estimate, 
                              using_exponent=True, min_periods=20, ew_lookback=250,
                              floor_at_zero=True):
    ## These may come from config as str
    using_exponent=str2Bool(using_exponent)
            
    if using_exponent:
        ## If we stack there will be duplicate dates
        ## So we massage the span so it's correct
        ## This assumes the index is at least daily and on same timestamp
        ## This is an artifact of how we prepare the data
        dindex=data_for_estimate.index
        dlenadj=float(len(dindex))/len(set(list(dindex)))
        ## Usual use for IDM, FDM calculation when whole data set is used
        corrmat=pd.ewmcorr(data_for_estimate, span=int(ew_lookback*dlenadj), min_periods=min_periods)
        
        ## only want the final one
        corrmat=corrmat.values[-1]
    else:
        ## Use normal correlation
        ## Usual use for bootstrapping when only have sub sample
        corrmat=data_for_estimate.corr(min_periods=min_periods)
        corrmat=corrmat.values

    if floor_at_zero:
        corrmat[corrmat<0]=0.0
    
    return corrmat

def fix_mus(mean_list):
    def _fixit(x):
        if np.isnan(x):
            return FLAG_BAD_RETURN
        else:
            return x    
    mean_list=[_fixit(x) for x in mean_list]    
    return mean_list

def fix_sigma(sigma):
    def _fixit(x):
        if np.isnan(x):
            return 0.0
        else:
            return x    
    sigma=[[_fixit(x) for x in sigma_row] for sigma_row in sigma]    
    sigma=np.array(sigma)    
    return sigma

def addem(weights):
    ## Used for constraints
    return 1.0 - sum(weights)

def neg_SR(weights, sigma, mus):
    ## Returns minus the Sharpe Ratio (as we're minimising)
    estreturn=(np.matrix(weights)*mus)[0,0]
    std_dev=(variance(weights,sigma)**.5)    
    return -estreturn/std_dev
    
def variance(weights, sigma):
    ## returns the variance (NOT standard deviation) given weights and sigma
    return (np.matrix(weights)*sigma*np.matrix(weights).transpose())[0,0]

def un_fix_weights(mean_list, weights):
    def _unfixit(xmean, xweight):
        if xmean==FLAG_BAD_RETURN:
            return np.nan
        else:
            return xweight
    
    fixed_weights=[_unfixit(xmean, xweight) for (xmean, xweight) in zip(mean_list, weights)]    
    return fixed_weights
    
def optimise( sigma, mean_list):
    
    ## will replace nans with big negatives
    mean_list=fix_mus(mean_list)
    
    ## replaces nans with zeros
    sigma=fix_sigma(sigma)
    
    mus=np.array(mean_list, ndmin=2).transpose()
    number_assets=sigma.shape[1]
    start_weights=[1.0/number_assets]*number_assets
    
    ## Constraints - positive weights, adding to 1.0
    bounds=[(0.0,1.0)]*number_assets
    cdict=[{'type':'eq', 'fun':addem}]
    ans=minimize(neg_SR, start_weights, (sigma, mus), method='SLSQP', bounds=bounds, constraints=cdict, tol=0.00001)

    ## anything that had a nan will now have a zero weight
    weights=ans['x']
    
    ## put back the nans
    weights=un_fix_weights(mean_list, weights)    
    return weights

def sigma_from_corr_and_std(stdev_list, corrmatrix):
    stdev=np.array(stdev_list, ndmin=2).transpose()
    sigma=stdev*corrmatrix*stdev
    return sigma
    
def markosolver(period_subset_data):
    mean_list=mean_estimator(period_subset_data)
    corrmatrix=correlation_single_period(period_subset_data)
    stdev_list=vol_estimator(period_subset_data)
    
    (mean_list, stdev_list)=vol_equaliser(mean_list, stdev_list)    
    sigma=sigma_from_corr_and_std(stdev_list, corrmatrix)    
    unclean_weights=optimise( sigma, mean_list)
    weights=unclean_weights    
    diag=dict(raw=(mean_list, stdev_list), sigma=sigma, mean_list=mean_list, 
              unclean=unclean_weights, weights=weights)    
    return (weights, diag)

def bootstrap_portfolio(subset_data, monte_runs=100, bootstrap_length=50):

    print ("bootstrap_portfolio=" + str(bootstrap_portfolio))
    all_results=[bs_one_time(subset_data, bootstrap_length) for unused_index in range(monte_runs)]
        
    ### We can take an average here; only because our weights always add
    ### up to 1. If that isn't true then you will need to some kind
    ### of renormalisation

    weightlist=np.array([x[0] for x in all_results], ndmin=2)
    diaglist=[x[1] for x in all_results]
         
    theweights_mean=list(np.mean(weightlist, axis=0))
    
    diag=dict(bootstraps=diaglist)
    
    return (theweights_mean, diag)

def bs_one_time(subset_data, bootstrap_length):

    ## choose the data    
    bs_idx=[int(random.uniform(0,1)*len(subset_data)) for notUsed in range(bootstrap_length)]
    
    returns=subset_data.iloc[bs_idx,:] 
    
    (weights, diag)=markosolver(returns)

    return (weights, diag)



def robust_vol_calc(x, days=35, min_periods=10, vol_abs_min=0.0000000001, vol_floor=True,
                    floor_min_quant=0.05, floor_min_periods=100,
                    floor_days=500):
    """
    Robust exponential volatility calculation, assuming daily series of prices
    We apply an absolute minimum level of vol (absmin);
    and a volfloor based on lowest vol over recent history

    :param x: data
    :type x: Tx1 pd.Series

    :param days: Number of days in lookback (*default* 35)
    :type days: int

    :param min_periods: The minimum number of observations (*default* 10)
    :type min_periods: int

    :param vol_abs_min: The size of absolute minimum (*default* =0.0000000001) 0.0= not used
    :type absmin: float or None

    :param vol_floor Apply a floor to volatility (*default* True)
    :type vol_floor: bool
    :param floor_min_quant: The quantile to use for volatility floor (eg 0.05 means we use 5% vol) (*default 0.05)
    :type floor_min_quant: float
    :param floor_days: The lookback for calculating volatility floor, in days (*default* 500)
    :type floor_days: int
    :param floor_min_periods: Minimum observations for floor - until reached floor is zero (*default* 100)
    :type floor_min_periods: int

    :returns: pd.DataFrame -- volatility measure


    """

    # Standard deviation will be nan for first 10 non nan values
    vol = pd.ewmstd(x, span=days, min_periods=min_periods)

    vol[vol < vol_abs_min] = vol_abs_min

    if vol_floor:
        # Find the rolling 5% quantile point to set as a minimum
        vol_min = pd.rolling_quantile(
            vol, floor_days, floor_min_quant, floor_min_periods)
        # set this to zero for the first value then propogate forward, ensures
        # we always have a value
        vol_min.set_value(vol_min.index[0], 0.0)
        vol_min = vol_min.ffill()

        # apply the vol floor
        vol_with_min = pd.concat([vol, vol_min], axis=1)
        vol_floored = vol_with_min.max(axis=1, skipna=False)
    else:
        vol_floored = vol

    return vol_floored

class fit_dates_object(object):
    def __init__(self, fit_start, fit_end, period_start, period_end, no_data=False):
        setattr(self, "fit_start", fit_start)
        setattr(self, "fit_end", fit_end)
        setattr(self, "period_start", period_start)
        setattr(self, "period_end", period_end)
        setattr(self, "no_data", no_data)
        
    def __repr__(self):
        if self.no_data:
            return "Fit without data, use from %s to %s" % (self.period_start, self.period_end)
        else:
            return "Fit from %s to %s, use in %s to %s" % (self.fit_start, self.fit_end, self.period_start, self.period_end)

def generate_fitting_dates(data, date_method, rollyears=20):
    """
    generate a list 4 tuples, one element for each year in the data
    each tuple contains [fit_start, fit_end, period_start, period_end] datetime objects
    the last period will be a 'stub' if we haven't got an exact number of years
    
    date_method can be one of 'in_sample', 'expanding', 'rolling'
    
    if 'rolling' then use rollyears variable  
    """

    print ("data=" + str(data.tail(4)))
    print ("date_method=" + str(date_method))
    if date_method not in ["in_sample","rolling", "expanding"]:
        raise Exception("don't recognise date_method %s should be one of in_sample, expanding, rolling" % date_method)
    
    if type(data) is list:
        start_date=min([dataitem.index[0] for dataitem in data])
        end_date=max([dataitem.index[-1] for dataitem in data])
    else:
        start_date=data.index[0]
        end_date=data.index[-1]

    ## now generate the dates we use to fit
    if date_method=="in_sample":
        ## single period
        return [fit_dates_object(start_date, end_date, start_date, end_date)]
    
    ## generate list of dates, one year apart, including the final date
    yearstarts=list(pd.date_range(start_date, end_date, freq="12M"))+[end_date]
   
    ## loop through each period     
    periods=[]
    for tidx in range(len(yearstarts))[1:-1]:
        ## these are the dates we test in
        period_start=yearstarts[tidx]
        period_end=yearstarts[tidx+1]

        ## now generate the dates we use to fit
        if date_method=="expanding":
            fit_start=start_date
        elif date_method=="rolling":
            yearidx_to_use=max(0, tidx-rollyears)
            fit_start=yearstarts[yearidx_to_use]
        else:
            raise Exception("don't recognise date_method %s should be one of in_sample, expanding, rolling" % date_method)
            
        if date_method in ['rolling', 'expanding']:
            fit_end=period_start
        else:
            raise Exception("don't recognise date_method %s " % date_method)
        
        periods.append(fit_dates_object(fit_start, fit_end, period_start, period_end))

    if date_method in ['rolling', 'expanding']:
        #add on a dummy date for the first year, when we have no data
        periods=[fit_dates_object(start_date, start_date, start_date, yearstarts[1], no_data=True)]+periods

    return periods
