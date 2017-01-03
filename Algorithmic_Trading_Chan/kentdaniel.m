clear;

load('inputDataOHLCDaily_stocks_20120424');

lookback=252;
holddays=25;
topN=50;

% idxStart=find(tday==20100104);
% idxEnd=find(tday==20120424);
idxStart=find(tday==20070515);
idxEnd=find(tday==20071231);
% idxStart=find(tday==20080102);
% idxEnd=find(tday==20091231);
% tday=tday(idxStart:idxEnd);
% cl=cl(idxStart:idxEnd, :);
% op=op(idxStart:idxEnd, :);

% cl is a TxN array of closing prices, where T is the number of trading
% days, and N is the number of stocks in the S&P 500
ret=(cl- backshift(lookback,cl))./backshift(lookback,cl); % daily returnslongs=false(size(ret));
shorts=false(size(ret));

positions=zeros(size(ret));
for t=lookback+1:length(tday)
   [foo idx]=sort(ret(t, :), 'ascend');
   nodata=find(isnan(ret(t, :)));
   idx=setdiff(idx, nodata, 'stable');
   longs(t, idx(end-topN+1:end))=true;
   shorts(t, idx(1:topN))=true;
end

for h=0:holddays-1
    long_lag=backshift(h, longs);
    long_lag(isnan(long_lag))=false;
    long_lag=logical(long_lag);
    
    short_lag=backshift(h, shorts);
    short_lag(isnan(short_lag))=false;
    short_lag=logical(short_lag);
    
    positions(long_lag)=positions(long_lag)+1;
    positions(short_lag)=positions(short_lag)-1;
end

dailyret=smartsum(backshift(1, positions).*(cl-lag(cl))./lag(cl), 2)/(2*topN)/holddays;

dailyret(isnan(dailyret))=0;

cumret=cumprod(1+dailyret(idxStart:idxEnd))-1;

plot(cumret);
tday=tday([idxStart:idxEnd]);

fprintf(1, 'Avg Ann Ret=%7.4f Sharpe ratio=%4.2f \n',252*smartmean(dailyret(idxStart:idxEnd)), sqrt(252)*smartmean(dailyret(idxStart:idxEnd))/smartstd(dailyret(idxStart:idxEnd)));
fprintf(1, 'APR=%10.4f\n', prod(1+dailyret(idxStart:idxEnd)).^(252/length(dailyret(idxStart:idxEnd)))-1);
[maxDD maxDDD]=calculateMaxDD(cumret);
fprintf(1, 'Max DD =%f Max DDD in days=%i\n\n', maxDD, round(maxDDD));
% Avg Ann Ret= 0.0315 Sharpe ratio=0.40 
% APR=    0.0288
% Max DD =-0.066923 Max DDD in days=182