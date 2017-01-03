clear;

topN=10; % Max number of positions
entryZscore=1;
lookback=20; % for MA

load('inputDataOHLCDaily_stocks_20120424', 'stocks', 'tday', 'op', 'hi', 'lo', 'cl');

stdretC2C90d=backshift(1, smartMovingStd(calculateReturns(cl, 1), 90));
buyPrice=backshift(1, lo).*(1-entryZscore*stdretC2C90d);

retGap=(op-backshift(1, lo))./backshift(1, lo);

pnl=zeros(length(tday), 1);

positionTable=zeros(size(cl));

ma=backshift(1, smartMovingAvg(cl, lookback));

for t=2:size(cl, 1)
    hasData=find(isfinite(retGap(t, :)) & op(t, :) < buyPrice(t, :) & op(t, :) > ma(t, :));
    
    [foo idxSort]=sort(retGap(t, hasData), 'ascend');
    positionTable(t, hasData(idxSort(1:min(topN, length(idxSort)))))=1;
end

retO2C=(cl-op)./op;


pnl=smartsum(positionTable.*(retO2C), 2);
ret=pnl/topN; 
ret(isnan(ret))=0;

fprintf(1, '%i - %i\n', tday(1), tday(end));
fprintf(1, 'APR=%10.4f\n', prod(1+ret).^(252/length(ret))-1);

fprintf(1, 'Sharpe=%4.2f\n', mean(ret)*sqrt(252)/std(ret));
% APR=8.7%, Sharpe=1.5

cumret=cumprod(1+ret)-1; % compounded ROE

plot(cumret);

