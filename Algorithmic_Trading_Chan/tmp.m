clear;

load('inputDataOHLCDaily_stocks_20120424', 'tday', 'stocks', 'op', 'cl');

e=load('earnannFile', 'earnann', 'tday');

[tday idx1 idx2]=intersect(tday, e.tday);
op=op(idx1, :);
cl=cl(idx1, :);
earnann=e.earnann(idx2, :);

tday(17)
stocks(5)
earnann(17,5)
%save('/tmp/A','earnann')

