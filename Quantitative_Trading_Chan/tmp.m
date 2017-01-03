clear;

lookback=252; % use lookback days as estimation (training) period for determining factor exposures.
numFactors=5;
topN=50; % for trading strategy, long stocks with topN expected 1-day returns.

load('IJR_20080114'); % test on SP600 smallcap stocks. (This MATLAB binary input file contains tday, stocks, op, hi, lo, cl arrays.

% tday, stocks, op, hi, lo, cl
disp(cl);
