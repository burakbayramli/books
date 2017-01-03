import zipfile, pandas as pd
import matplotlib.pyplot as plt
import numpy as np, random, datetime
from scipy.optimize import minimize

def create_dull_pd_matrix(dullvalue=0.0, dullname="A",
                          startdate=pd.datetime(1970,1,1).date(),
                          enddate=datetime.datetime.now().date(), index=None):
    if index is None:
        index=pd.date_range(startdate, enddate)    
    dullvalue=np.array([dullvalue]*len(index))    
    ans=pd.DataFrame(dullvalue, index, columns=[dullname])    
    return ans

def addem(weights):
    return 1.0 - sum(weights)

def variance(weights, sigma):
    return (np.matrix(weights)*sigma*np.matrix(weights).transpose())[0,0]

def neg_SR(weights, sigma, mus):
    estreturn=(np.matrix(weights)*mus)[0,0]
    std_dev=(variance(weights,sigma)**.5)    
    return -estreturn/std_dev

def equalise_vols(returns, default_vol):    
    factors=(default_vol/16.0)/returns.std(axis=0)
    facmat=create_dull_pd_matrix(dullvalue=factors,
                                 dullname=returns.columns,
                                 index=returns.index)
    norm_returns=returns*facmat
    norm_returns.columns=returns.columns
    return norm_returns

def markosolver(returns, default_vol, default_SR):        
    use_returns=equalise_vols(returns, default_vol)    
    sigma=use_returns.cov().values
    mus=np.array([use_returns[asset_name].mean() for asset_name in use_returns.columns], ndmin=2)
    mus=mus.transpose()
    number_assets=use_returns.shape[1]
    start_weights=[1.0/number_assets]*number_assets    
    bounds=[(0.0,1.0)]*number_assets
    cdict=[{'type':'eq', 'fun':addem}]    
    ans=minimize(neg_SR, start_weights,
                 (sigma, mus),
                 method='SLSQP',
                 bounds=bounds,
                 constraints=cdict,
                 tol=0.00001)
    return ans['x']

def generate_fitting_dates(data, rollyears):
    start_date=data.index[0]
    end_date=data.index[-1]
    yearstarts=list(pd.date_range(start_date, end_date, freq="12M"))+[end_date]   
    periods=[]
    for tidx in range(len(yearstarts))[1:-1]:
        period_start=yearstarts[tidx]
        period_end=yearstarts[tidx+1]
        fit_start=start_date            
        fit_end=period_start        
        periods.append([fit_start, fit_end, period_start, period_end])

    return periods

def bootstrap_portfolio(returns_to_bs, monte_carlo, monte_length, default_vol, default_SR):
            
    weightlist=[]
    for unused_index in range(monte_carlo):
        bs_idx=[int(random.uniform(0,1)*len(returns_to_bs)) for i in range(monte_length)]        
        returns=returns_to_bs.iloc[bs_idx,:] 
        weight=markosolver(returns, default_vol=default_vol, default_SR=default_SR)
        weightlist.append(weight)
     
    theweights_mean=list(np.mean(weightlist, axis=0))
    return theweights_mean

def optimise_over_periods(data,
                          rollyears=20, 
                          monte_carlo=40,
                          monte_length=250):

    fit_periods=generate_fitting_dates(data, rollyears=rollyears)    
    weight_list=[]
    for fit_tuple in fit_periods:
        print ("fit_tuple=" + str(fit_tuple))
        period_subset_data=data[fit_tuple[0]:fit_tuple[1]]        
        weights=bootstrap_portfolio(period_subset_data,
                                    monte_carlo=monte_carlo,
                                    monte_length=monte_length,
                                    default_vol=0.2, default_SR=1.0 )
        
        dindex=[fit_tuple[2]+datetime.timedelta(seconds=1),
                fit_tuple[3]-datetime.timedelta(seconds=1)] 
        weight_row=pd.DataFrame([weights]*2,
                                index=dindex,
                                columns=data.columns) 
        weight_list.append(weight_row)
        
    weight_df=pd.concat(weight_list, axis=0)    
    return weight_df

if __name__ == "__main__": 
 
base = "../pysystemtrade/sysdata/legacycsv"
    random.seed(0)

    df =  pd.read_csv('%s/SP500_price.csv' % base,sep=',',index_col=0,parse_dates=True)
    df['NASDAQ'] =  pd.read_csv('%s/NASDAQ_price.csv' % base,sep=',',index_col=0,parse_dates=True)
    df['US20'] =  pd.read_csv('%s/US20_price.csv' % base,sep=',',index_col=0,parse_dates=True)
    df.columns = ['SP500','NASDAQ','US20']

    df['SP500'] = df.SP500.pct_change()
    df['NASDAQ'] = df.NASDAQ.pct_change()
    df['US20'] = df.US20.pct_change()

    df = df[(df.index >= '1999-08-02') & (df.index <= '2015-04-22')]

    mat1=optimise_over_periods(df)
    mat1.plot()
    plt.show()
