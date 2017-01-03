clear;

load('inputDataOHLCDaily_stocks_20120424', 'tday', 'stocks', 'op', 'cl');

e=load('earnannFile', 'earnann', 'tday');

[tday idx1 idx2]=intersect(tday, e.tday);
op=op(idx1, :);
cl=cl(idx1, :);
earnann=e.earnann(idx2, :);

lookback=90;

retC2O=(op-backshift(1, cl))./backshift(1, cl);
stdC2O=smartMovingStd(retC2O, lookback);

positions=zeros(size(cl));

longs=retC2O >= 0.5*stdC2O & earnann;
shorts=retC2O <= -0.5*stdC2O & earnann;

positions(longs)=1;
positions(shorts)=-1;

ret=smartsum(positions.*(cl-op)./op, 2)/30;
cumret=cumprod(1+ret)-1;

plot(cumret);

fprintf(1, 'Avg Ann Ret=%7.4f Sharpe ratio=%4.2f \n',252*smartmean(ret), sqrt(252)*smartmean(ret)/smartstd(ret));
fprintf(1, 'APR=%10.4f\n', prod(1+ret).^(252/length(ret))-1);
[maxDD maxDDD]=calculateMaxDD(cumret);
fprintf(1, 'Max DD =%f Max DDD in days=%i\n\n', maxDD, round(maxDDD));
% Avg Ann Ret= 0.0667 Sharpe ratio=1.49 
% APR=    0.0680
% Max DD =-0.026052 Max DDD in days=109
