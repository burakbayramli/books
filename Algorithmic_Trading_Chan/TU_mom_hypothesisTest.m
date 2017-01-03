clear;

% load('../Data/inputDataOHLCDaily_20120511', 'syms', 'tday', 'cl');
load('inputDataOHLCDaily_20120511', 'syms', 'tday', 'cl');
idx=strmatch('TU', syms, 'exact');

tday=tday(:, idx);
cl=cl(:, idx);

lookback=250;
holddays=25;

longs=cl > backshift(lookback, cl)  ;
shorts=cl < backshift(lookback, cl) ;

pos=zeros(length(cl), 1);

for h=0:holddays-1
    long_lag=backshift(h, longs);
    long_lag(isnan(long_lag))=false;
    long_lag=logical(long_lag);
    
    short_lag=backshift(h, shorts);
    short_lag(isnan(short_lag))=false;
    short_lag=logical(short_lag);
    
    pos(long_lag)=pos(long_lag)+1;
    pos(short_lag)=pos(short_lag)-1;
end

marketRet=(cl-backshift(1, cl))./backshift(1, cl);
marketRet(~isfinite(marketRet))=0;

ret=backshift(1, pos).*marketRet/holddays;

ret(isnan(ret))=0;

% Gaussian hypothesis test
fprintf(1, 'Gaussian Test statistic=%4.2f\n', mean(ret)/std(ret)*sqrt(length(ret)));
% Gaussian Test statistic=2.93

% Randomized market returns hypothesis test
moments={mean(marketRet), std(marketRet), skewness(marketRet), kurtosis(marketRet)};
numSampleAvgretBetterOrEqualObserved=0;
for sample=1:10000
    marketRet_sim=pearsrnd(moments{:}, length(marketRet), 1);
    cl_sim=cumprod(1+marketRet_sim)-1;
    
    longs_sim=cl_sim > backshift(lookback, cl_sim)  ;
    shorts_sim=cl_sim < backshift(lookback, cl_sim) ;
    
    pos_sim=zeros(length(cl_sim), 1);
    
    for h=0:holddays-1
        long_sim_lag=backshift(h, longs_sim);
        long_sim_lag(isnan(long_sim_lag))=false;
        long_sim_lag=logical(long_sim_lag);
        
        short_sim_lag=backshift(h, shorts_sim);
        short_sim_lag(isnan(short_sim_lag))=false;
        short_sim_lag=logical(short_sim_lag);
        
        pos_sim(long_sim_lag)=pos_sim(long_sim_lag)+1;
        pos_sim(short_sim_lag)=pos_sim(short_sim_lag)-1;
    end

    
    ret_sim=backshift(1, pos_sim).*marketRet_sim/holddays;
    ret_sim(~isfinite(ret_sim))=0;
    
    if (mean(ret_sim)>= mean(ret))
        numSampleAvgretBetterOrEqualObserved=numSampleAvgretBetterOrEqualObserved+1;
    end
end

fprintf(1, 'Randomized prices: p-value=%f\n', numSampleAvgretBetterOrEqualObserved/10000);
% p-value=0.027500


% Randomized entry trades hypothesis test

numSampleAvgretBetterOrEqualObserved=0;
for sample=1:100000
    P=randperm(length(longs));
    longs_sim=longs(P);
    shorts_sim=shorts(P);
    
    pos_sim=zeros(length(cl), 1);

    for h=0:holddays-1
        long_sim_lag=backshift(h, longs_sim);
        long_sim_lag(isnan(long_sim_lag))=false;
        long_sim_lag=logical(long_sim_lag);
        
        short_sim_lag=backshift(h, shorts_sim);
        short_sim_lag(isnan(short_sim_lag))=false;
        short_sim_lag=logical(short_sim_lag);
        
        pos(long_sim_lag)=pos(long_sim_lag)+1;
        pos(short_sim_lag)=pos(short_sim_lag)-1;
    end

    ret_sim=backshift(1, pos_sim).*marketRet/holddays;

    ret_sim(isnan(ret_sim))=0;
    
  
    if (mean(ret_sim)>= mean(ret))
        numSampleAvgretBetterOrEqualObserved=numSampleAvgretBetterOrEqualObserved+1;
    end

end
fprintf(1, 'Randomized trades: p-value=%f\n', numSampleAvgretBetterOrEqualObserved/100000);
