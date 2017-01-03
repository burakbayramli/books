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

load('IJR_20080131');
onewaytcost=0.0005; % 5bp one way transaction cost

years=year(cellstr(datestr(datenum(cellstr(num2str(tday)), 'yyyymmdd'))));
months=month(cellstr(datestr(datenum(cellstr(num2str(tday)), 'yyyymmdd'))));

nextdayyear=fwdshift(1, years);
nextdaymonth=fwdshift(1, months);

lastdayofDec=find(months==12 & nextdaymonth==1);
lastdayofJan=find(months==1 & nextdaymonth==2);

lastdayofJan(1)=[]; % lastdayofDec starts in 2004, so remove 2004 from lastdayofJan
assert(all(tday(lastdayofJan) > tday(lastdayofDec))); % Ensure each lastdayofJan date after each lastdayofDec date

eoy=find(years~=nextdayyear); % End Of Year indices

eoy(end)=[]; % last index is not End of Year

assert(all(tday(eoy)==tday(lastdayofDec))); % Ensure eoy dates match lastdayofDec dates

annret=(cl(eoy(2:end), :)-cl(eoy(1:end-1), :))./cl(eoy(1:end-1), :); % annual returns
janret=(cl(lastdayofJan(2:end), :)-cl(lastdayofDec(2:end), :))./cl(lastdayofDec(2:end), :); % January returns

for y=1:size(annret, 1)
    hasData=find(isfinite(annret(y, :))); % pick those stocks with valid annual returns
    [foo sortidx]=sort(annret(y, hasData), 'ascend'); % sort stocks based on prior year's returns
    
    topN=round(length(hasData)/10); % buy stocks with lowest decile of returns, and vice versa for highest decile
    
    portRet=(smartmean(janret(y, hasData(sortidx(1:topN))))-smartmean(janret(y, hasData(sortidx(end-topN+1:end)))))/2-2*onewaytcost; % portfolio returns
    fprintf(1,'Last holding date %i: Portfolio return=%7.4f\n', tday(lastdayofDec(y+1)), portRet);
end
% These should be the output
% Last holding date 20051230: Portfolio return=-0.0244
% Last holding date 20061229: Portfolio return=-0.0068
% Last holding date 20071231: Portfolio return= 0.0881
