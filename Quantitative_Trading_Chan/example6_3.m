% 
% written by:
% Ernest Chan
%
% Author of Quantitative Trading: 
% How to Start Your Own Algorithmic Trading Business”
%
% ernest@epchan.com
% www.epchan.com

clear; % make sure previously defined variables are erased.
 
[num1, txt1]=xlsread('OIH'); % read a spreadsheet named "OIH.xls" into MATLAB. 
 
tday1=txt1(2:end, 1); % the first column (starting from the second row) is the trading days in format mm/dd/yyyy.
 
tday1=datestr(datenum(tday1, 'mm/dd/yyyy'), 'yyyymmdd'); % convert the format into yyyymmdd.
 
tday1=str2double(cellstr(tday1)); % convert the date strings first into cell arrays and then into numeric format.
 
adjcls1=num1(:, end); % the last column contains the adjusted close prices.
 
[num2, txt2]=xlsread('RKH'); % read a spreadsheet named "RKH.xls" into MATLAB. 
 
tday2=txt2(2:end, 1); % the first column (starting from the second row) is the trading days in format mm/dd/yyyy.
 
tday2=datestr(datenum(tday2, 'mm/dd/yyyy'), 'yyyymmdd'); % convert the format into yyyymmdd.
 
tday2=str2double(cellstr(tday2)); % convert the date strings first into cell arrays and then into numeric format.

adjcls2=num2(:, end);

[num3, txt3]=xlsread('RTH'); % read a spreadsheet named "RTH.xls" into MATLAB. 
 
tday3=txt3(2:end, 1); % the first column (starting from the second row) is the trading days in format mm/dd/yyyy.
 
tday3=datestr(datenum(tday3, 'mm/dd/yyyy'), 'yyyymmdd'); % convert the format into yyyymmdd.
 
tday3=str2double(cellstr(tday3)); % convert the date strings first into cell arrays and then into numeric format.

adjcls3=num3(:, end);

% merge these data
tday=union(tday1, tday2);
tday=union(tday, tday3);
adjcls=NaN(length(tday), 3);

[foo idx1 idx]=intersect(tday1, tday);
adjcls(idx, 1)=adjcls1(idx1);
[foo idx2 idx]=intersect(tday2, tday);
adjcls(idx, 2)=adjcls2(idx2);
[foo idx3 idx]=intersect(tday3, tday);
adjcls(idx, 3)=adjcls3(idx3);

ret=(adjcls-lag1(adjcls))./lag1(adjcls); % returns

baddata=find(any(~isfinite(ret), 2)); % days where any one return is missing

ret(baddata, :)=[]; % eliminate days where any one return is missing

excessRet=ret-repmat(0.04/252, size(ret)); % excess returns: assume annualized risk free rate is 4%

M=252*mean(excessRet, 1)' % annualized mean excess returns
% M =
% 
%     0.1396
%     0.0294
%    -0.0073

C=252*cov(excessRet) % annualized covariance matrix
% C =
% 
%     0.1109    0.0200    0.0183
%     0.0200    0.0372    0.0269
%     0.0183    0.0269    0.0420

F=inv(C)*M % Kelly optimal leverages
% F =
% 
%     1.2919
%     1.1723
%    -1.4882

g=0.04+F'*C*F/2 % Maximum annualized compounded growth rate
% g =
% 
%     0.1529

S=sqrt(F'*C*F) % Sharpe ratio of portfolio
% S =
% 
%     0.4751
