clear;

% gc=load('//dellquad/Futures_data/inputData_GC_1600_20100802', 'tday', 'hhmm', 'cl'); 
gc=load('inputData_GC_1600_20100802', 'tday', 'hhmm', 'cl'); 
gld=load('inputData_ETF');

gld.cl=gld.cl(:, strcmp('GLD', gld.syms));

[tday idx1 idx2]=intersect(gc.tday, gld.tday);

gc.cl=gc.cl(idx1);
gld.cl=gld.cl(idx2);

% Long GLD and short GC
ret=(gld.cl-lag(gld.cl, 1))./lag(gld.cl, 1)-(gc.cl-lag(gc.cl, 1))./lag(gc.cl, 1);

ret(isnan(ret))=0;

cumret=cumprod(1+ret)-1;

plot(cumret);

riskFreeRate=0.02/252;
fprintf(1, 'Avg Ann Ret=%7.4f Sharpe ratio=%4.2f \n',252*smartmean(ret), sqrt(252)*smartmean(ret-riskFreeRate)/smartstd(ret-riskFreeRate));
fprintf(1, 'APR=%10.4f\n', prod(1+ret).^(252/length(ret))-1);
[maxDD maxDDD]=calculateMaxDD(cumret);
fprintf(1, 'Max DD =%f Max DDD in days=%i\n\n', maxDD, round(maxDDD));
% 
% Avg Ann Ret= 0.0190 Sharpe ratio=-.07
% APR=    0.0191
% Max DD =-0.008247 Max DDD in days=91