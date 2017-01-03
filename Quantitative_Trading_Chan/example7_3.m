% 
% written by:
% Ernest Chan
%
% Author of “Quantitative Trading: 
% How to Start Your Own Algorithmic Trading Business”
%
% ernest@epchan.com
% www.epchan.com

clear; % make sure previously defined variables are erased.
 
[num, txt]=xlsread('KO'); % read a spreadsheet named "KO.xls" into MATLAB. 
 
tday1=txt(2:end, 1); % the first column (starting from the second row) is the trading days in format mm/dd/yyyy.
 
tday1=datestr(datenum(tday1, 'mm/dd/yyyy'), 'yyyymmdd'); % convert the format into yyyymmdd.
 
tday1=str2double(cellstr(tday1)); % convert the date strings first into cell arrays and then into numeric format.
 
adjcls1=num(:, end); % the last column contains the adjusted close prices.
 
[num2, txt2]=xlsread('PEP'); % read a spreadsheet named "PEP.xls" into MATLAB. 
 
tday2=txt2(2:end, 1); % the first column (starting from the second row) is the trading days in format mm/dd/yyyy.
 
tday2=datestr(datenum(tday2, 'mm/dd/yyyy'), 'yyyymmdd'); % convert the format into yyyymmdd.
 
tday2=str2double(cellstr(tday2)); % convert the date strings first into cell arrays and then into numeric format.

adjcls2=num2(:, end);

tday=union(tday1, tday2); % find all the days when either KO or PEP has data.

[foo idx idx1]=intersect(tday, tday1);

adjcls=NaN(length(tday), 2); % combining the two price series

adjcls(idx, 1)=adjcls1(idx1);

[foo idx idx2]=intersect(tday, tday2);

adjcls(idx, 2)=adjcls2(idx2);

baddata=find(any(~isfinite(adjcls), 2)); % days where any one price is missing

tday(baddata)=[];

adjcls(baddata, :)=[];

vnames=strvcat('KO', 'PEP');

res=cadf(adjcls(:, 1), adjcls(:, 2), 0, 1); % run cointegration check using augmented Dickey-Fuller test

prt(res, vnames); 

% Output from cadf function:

%  Augmented DF test for co-integration variables:                        KO ,PEP  
% CADF t-statistic        # of lags   AR(1) estimate 
%      -2.14258438                1        -0.001167 
% 
%    1% Crit Value    5% Crit Value   10% Crit Value 
%           -3.880           -3.359           -3.038 


% The t-statistic of -2.14 is larger than the 10% Crit Value of -3.038
% which means that there is a less than 90%
% probability that these 2 time series are cointegrated.

results=ols(adjcls(:, 1), adjcls(:, 2)); 

hedgeRatio=results.beta
z=results.resid;

% A hedgeRatio of 1.0114 was found. I.e. KO=1.0114*PEP + z, where z can be interpreted as the
% spread KO-1.0114*PEP, which is not stationary here.

plot(z); % This should produce a chart similar to Figure 7.2 (except for the titles and labels).

% A test for correlation.
dailyReturns=(adjcls-lag1(adjcls))./lag1(adjcls);
[R,P]=corrcoef(dailyReturns(2:end, :));

% R =
% 
%     1.0000    0.4849
%     0.4849    1.0000
% 
% 
% P =
% 
%      1     0
%      0     1

% The P value of 0 indicates that the two time series are significantly
% correlated.

