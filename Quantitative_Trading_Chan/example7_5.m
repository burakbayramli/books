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
 
[num, txt]=xlsread('GLD'); % read a spreadsheet named "GLD.xls" into MATLAB. 
 
tday1=txt(2:end, 1); % the first column (starting from the second row) is the trading days in format mm/dd/yyyy.
 
tday1=datestr(datenum(tday1, 'mm/dd/yyyy'), 'yyyymmdd'); % convert the format into yyyymmdd.
 
tday1=str2double(cellstr(tday1)); % convert the date strings first into cell arrays and then into numeric format.
 
adjcls1=num(:, end); % the last column contains the adjusted close prices.
  
[num2, txt2]=xlsread('GDX'); % read a spreadsheet named "GDX.xls" into MATLAB. 
 
tday2=txt2(2:end, 1); % the first column (starting from the second row) is the trading days in format mm/dd/yyyy.
 
tday2=datestr(datenum(tday2, 'mm/dd/yyyy'), 'yyyymmdd'); % convert the format into yyyymmdd.
 
tday2=str2double(cellstr(tday2)); % convert the date strings first into cell arrays and then into numeric format.

adjcls2=num2(:, end);

tday=union(tday1, tday2); % find all the days when either GLD or GDX has data.

[foo idx idx1]=intersect(tday, tday1);

adjcls=NaN(length(tday), 2); % combining the two price series

adjcls(idx, 1)=adjcls1(idx1);

[foo idx idx2]=intersect(tday, tday2);

adjcls(idx, 2)=adjcls2(idx2);

baddata=find(any(~isfinite(adjcls), 2)); % days where any one price is missing

tday(baddata)=[];

adjcls(baddata, :)=[];

results=ols(adjcls(:, 1), adjcls(:, 2)); 

hedgeRatio=results.beta
z=results.resid; % residuals: deviation from the mean value of the spread

% A hedgeRatio of 1.6766 was found. I.e. GLD=1.6766*GDX + z, where z can be interpreted as the
% spread GLD-1.6766*GDX and should be stationary.

% ====== same as example7_1.m up to here ======

prevz=backshift(1, z); % z at a previous time-step
dz=z-prevz;
dz(1)=[];
prevz(1)=[];
results=ols(dz, prevz-mean(prevz)); % assumes dz=theta*(z-mean(z))dt+w, where w is error term
theta=results.beta;

halflife=-log(2)/theta

% halflife =
% 
%    10.0037

