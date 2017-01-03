clear;

load('inputDataOHLCDaily_20120511', 'syms', 'tday', 'cl');
% load('//dellquad/Futures_data/inputDataOHLCDaily_20120815', 'syms', 'tday', 'cl');
idx=strmatch('TU', syms, 'exact');

tday=tday(:, idx);
cl=cl(:, idx);

% Correlation tests
for lookback=[1 5 10 25 60 120 250]
    for holddays=[1 5 10 25 60 120 250]
        ret_lag=(cl-backshift(lookback, cl))./backshift(lookback, cl);
        ret_fut=(fwdshift(holddays, cl)-cl)./cl;
        badDates=any([isnan(ret_lag) isnan(ret_fut)], 2);
        ret_lag(badDates)=[];
        ret_fut(badDates)=[];
        
        if (lookback >= holddays)
            indepSet=[1:holddays:length(ret_lag)];
        else
            indepSet=[1:lookback:length(ret_lag)];
        end

        ret_lag=ret_lag(indepSet);
        ret_fut=ret_fut(indepSet);
            
        [cc, pval]=corrcoef(ret_lag, ret_fut);
        %         fprintf(1, 'lookback=%3i holddays=%3i cc=%7.4f pval=%6.4f\n', lookback, holddays, cc(1, 2), pval(1, 2));
        fprintf(1, '%3i\t%3i\t%7.4f\t%6.4f\n',  lookback, holddays, cc(1, 2), pval(1, 2));
    end
end

% Hurst exponent and Variance ratio test
H=genhurst(log(cl), 2);
fprintf(1, 'H2=%f\n', H);

% Variance ratio test from Matlab Econometrics Toolbox
[h,pValue]=vratiotest(log(cl));

fprintf(1, 'h=%i\n', h); % h=1 means rejection of random walk hypothesis, 0 means it is a random walk.
fprintf(1, 'pValue=%f\n', pValue); % pValue is essentially the probability that the null hypothesis (random walk) is true.

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

ret=(backshift(1, pos).*(cl-backshift(1, cl))./backshift(1, cl))/holddays;

ret(isnan(ret))=0;
% idx=find(tday==20090102);
idx=1;

cumret=cumprod(1+ret(idx:end))-1;

plot(cumret);

fprintf(1, 'Avg Ann Ret=%7.4f Ann Volatility=%7.4f Sharpe ratio=%4.2f \n',252*smartmean(ret(idx:end)), sqrt(252)*smartstd(ret(idx:end)), sqrt(252)*smartmean(ret(idx:end))/smartstd(ret(idx:end)));
fprintf(1, 'APR=%10.4f\n', prod(1+ret(idx:end)).^(252/length(ret(idx:end)))-1);
[maxDD maxDDD]=calculateMaxDD(cumret);
fprintf(1, 'Max DD =%f Max DDD in days=%i\n\n', maxDD, round(maxDDD));
fprintf(1, 'Kelly f=%f\n', mean(ret(idx:end))/std(ret(idx:end))^2);

% Avg Ann Ret= 0.0167 Sharpe ratio=1.04 
% APR=    0.0167
% Max DD =-0.024847 Max DDD in days=343
% Kelly f=64.919535


