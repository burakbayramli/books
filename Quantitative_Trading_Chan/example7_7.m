% 
% written by:
% Ernest Chan
%
% Author of “Quantitative Trading: 
% How to Start Your Own Algorithmic Trading Business”
%
% ernest@epchan.com
% www.epchan.com

clear;

load('SPX_20071123', 'tday', 'stocks', 'cl');

monthEnds=find(isLastTradingDayOfMonth(tday)); % find the indices of the days that are at month ends.

monthlyRet=(cl(monthEnds, :)-lag1(cl(monthEnds, :)))./lag1(cl(monthEnds, :));

mycl=fillMissingData(cl);

% myMonthlyRet=(mycl(monthEnds, :)-lag1(mycl(monthEnds, :)))./lag1(mycl(monthEnds, :));

[monthlyRetSorted sortIndex]=sort(monthlyRet, 2); % sort stocks by monthly returns in ascending order

prevYearMonthlyRetSorted=backshift(12, monthlyRetSorted); % these are the sorted monthly returns of the previous year
prevYearSortIndex=backshift(12, sortIndex); % the sort index of the previous year

positions=zeros(size(monthlyRet));

for m=13:size(monthlyRet, 1)
    hasReturns=isfinite(prevYearMonthlyRetSorted(m, :)) & isfinite(cl(monthEnds(m-1), :));
    mySortIndex=prevYearSortIndex(m, hasReturns);
    
    topN=floor(length(mySortIndex)/10); % take top decile of stocks as longs, bottom decile as shorts 
    
    positions(m-1, mySortIndex(1:topN))=-1;
    positions(m-1, mySortIndex(end-topN+1:end))=1;       
end

ret=smartsum(lag1(positions).*monthlyRet, 2);

avgannret=12*smartmean(ret);
sharpe=sqrt(12)*smartmean(ret)/smartstd(ret);

fprintf(1, 'Avg ann return=%7.4f Sharpe ratio=%7.4f\n', avgannret, sharpe);
% Output should be
% Avg ann return=-0.9167 Sharpe ratio=-0.1055
