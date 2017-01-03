import inspect
import inspect
import sys; sys.path.append('../..')
from systems.provided.futures_chapter15.basesystem import futures_system
import pandas as pd, numpy as np, random, datetime
from syscore.objects import update_recalc, resolve_function
from syscore.genutils import str2Bool, group_dict_from_natural
from syscore.pdutils import df_from_list, must_have_item
from copy import copy

def get_avg_corr(sigma):
    new_sigma=copy(sigma)
    np.fill_diagonal(new_sigma,np.nan)
    if np.all(np.isnan(new_sigma)):
        return np.nan    
    avg_corr=np.nanmean(new_sigma)
    return avg_corr

def correlation_single_period(data_for_estimate, 
                              using_exponent=True,
                              min_periods=20,
                              ew_lookback=250,
                              floor_at_zero=True):

    using_exponent=str2Bool(using_exponent)            
    if using_exponent:
        dindex=data_for_estimate.index
        dlenadj=float(len(dindex))/len(set(list(dindex)))
        corrmat=pd.ewmcorr(data_for_estimate, span=int(ew_lookback*dlenadj), min_periods=min_periods)
        corrmat=corrmat.values[-1]
    else:
        corrmat=data_for_estimate.corr(min_periods=min_periods)
        corrmat=corrmat.values
    if floor_at_zero:
        corrmat[corrmat<0]=0.0
    return corrmat

def clean_correlation(corrmat, corr_with_no_data, must_haves=None):
    if must_haves is None:
        must_haves=[True]*corrmat.shape[0]
    if not np.any(np.isnan(corrmat)):
        return corrmat
    if np.all(np.isnan(corrmat)):
        return corr_with_no_data
    size_range=range(corrmat.shape[0])
    avgcorr=get_avg_corr(corrmat)
    def _good_correlation(i,j,corrmat, avgcorr, must_haves, corr_with_no_data):
        value=corrmat[i][j]
        must_have_value=must_haves[i] and must_haves[j]
        
        if np.isnan(value):
            if must_have_value:
                return avgcorr
            else:
                return corr_with_no_data[i][j]
        else:
            return value

    corrmat=np.array([[_good_correlation(i,j, corrmat, avgcorr, must_haves,corr_with_no_data) 
                       for i in size_range] for j in size_range], ndmin=2)
    np.fill_diagonal(corrmat,1.0)    
    return corrmat

def boring_corr_matrix(size, offdiag=0.99, diag=1.0):
    size_index=range(size)
    def _od(offdag, i, j):
        if i==j:
            return diag
        else:
            return offdiag
    m= [[_od(offdiag, i,j) for i in size_index] for j in size_index]
    m=np.array(m)
    return m

class CorrelationList(object):
    def __init__(self, corr_list, column_names, fit_dates):
        setattr(self, "corr_list", corr_list)
        setattr(self, "columns", column_names)
        setattr(self, "fit_dates", fit_dates)
    def __repr__(self):
        return "%d correlation estimates for %s" % (len(self.corr_list), ",".join(self.columns))
    
class CorrelationEstimator(CorrelationList):

    def __init__(self, data, frequency="W",
                 date_method="expanding", rollyears=20, 
                 dict_group=dict(), boring_offdiag=0.99, cleaning=True, **kwargs):
        cleaning=str2Bool(cleaning)
        group_dict=group_dict_from_natural(dict_group)
        data=df_from_list(data)    
        column_names=list(data.columns)
        data=data.resample(frequency, how="last")
        fit_dates = generate_fitting_dates(data, date_method=date_method, rollyears=rollyears)
        size=len(column_names)
        corr_with_no_data=boring_corr_matrix(size, offdiag=boring_offdiag)        
        corr_list=[]        
        for fit_period in fit_dates:
            print( (fit_period.period_start, fit_period.period_end))
            if fit_period.no_data:
                corr_with_nan=boring_corr_matrix(size, offdiag=np.nan, diag=np.nan)
                corrmat=corr_with_nan                
            else:                
                data_for_estimate=data[fit_period.fit_start:fit_period.fit_end]  
                corrmat=correlation_single_period(data_for_estimate, **kwargs)

            if cleaning:
                current_period_data=data[fit_period.fit_start:fit_period.fit_end] 
                must_haves=must_have_item(current_period_data)
                corrmat=clean_correlation(corrmat, corr_with_no_data, must_haves) 

            corr_list.append(corrmat)
        
        setattr(self, "corr_list", corr_list)
        setattr(self, "columns", column_names)
        setattr(self, "fit_dates", fit_dates)

def generate_fitting_dates(data, date_method, rollyears=20):

    print ("date_method=" + str(date_method))
    if date_method not in ["in_sample","rolling", "expanding"]:
        raise Exception("don't recognise date_method %s should be one of in_sample, expanding, rolling" % date_method)
    
    if type(data) is list:
        start_date=min([dataitem.index[0] for dataitem in data])
        end_date=max([dataitem.index[-1] for dataitem in data])
    else:
        start_date=data.index[0]
        end_date=data.index[-1]

    if date_method=="in_sample":
        return [fit_dates_object(start_date, end_date, start_date, end_date)]

    yearstarts=list(pd.date_range(start_date, end_date, freq="12M"))+[end_date]

    periods=[]
    for tidx in range(len(yearstarts))[1:-1]:
        period_start=yearstarts[tidx]
        period_end=yearstarts[tidx+1]
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
        periods=[fit_dates_object(start_date, start_date, start_date, yearstarts[1], no_data=True)]+periods

    return periods

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
        
if __name__ == "__main__": 

    dfs = []
    system=futures_system()
    insts = ['EDOLLAR','US10','EUROSTX','V2X','MXP','CORN']
    dfs = []
    for c in insts:
        df = system.accounts.pandl_for_subsystem(c)
        dfs.append(df)
    pandl = pd.concat(dfs,axis=1)        
    frequency="W"
    pandl=pandl.cumsum().resample(frequency).diff()
    res = CorrelationEstimator(pandl, frequency=frequency,
                               ew_lookback=500, floor_at_zero=True,
                               min_periods=20,
                               cleaning=True,
                               using_exponent=True,
                               #using_exponent=False,
                               date_method='expanding', rollyears=20)
    res = pd.DataFrame(res.corr_list[-1], index=insts)
    res.columns = insts
    print (res)

# using_exponent=True    
#           EDOLLAR      US10   EUROSTX       V2X       MXP      CORN
# EDOLLAR  1.000000  0.896486  0.000000  0.000000  0.000000  0.000000
# US10     0.896486  1.000000  0.000000  0.000000  0.000000  0.021427
# EUROSTX  0.000000  0.000000  1.000000  0.565125  0.000000  0.019574
# V2X      0.000000  0.000000  0.565125  1.000000  0.174376  0.064895
# MXP      0.000000  0.000000  0.000000  0.174376  1.000000  0.000000
# CORN     0.000000  0.021427  0.019574  0.064895  0.000000  1.000000

# using_exponent=False
#           EDOLLAR      US10   EUROSTX       V2X       MXP      CORN
# EDOLLAR  1.000000  0.831309  0.000000  0.000000  0.000000  0.000000
# US10     0.831309  1.000000  0.000000  0.000000  0.000000  0.011105
# EUROSTX  0.000000  0.000000  1.000000  0.582887  0.000000  0.017221
# V2X      0.000000  0.000000  0.582887  1.000000  0.188938  0.058914
# MXP      0.000000  0.000000  0.000000  0.188938  1.000000  0.000000
# CORN     0.000000  0.011105  0.017221  0.058914  0.000000  1.000000

