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

[num, txt]=xlsread('GDX'); % read a spreadsheet named "GDX.xls" into MATLAB. 

tday2=txt(2:end, 1); % the first column (starting from the second row) is the trading days in format mm/dd/yyyy.

tday2=datestr(datenum(tday2, 'mm/dd/yyyy'), 'yyyymmdd'); % convert the format into yyyymmdd.

tday2=str2double(cellstr(tday2)); % convert the date strings first into cell arrays and then into numeric format.

adjcls2=num(:, end); % the last column contains the adjusted close prices.

[tday, idx1, idx2]=intersect(tday1, tday2); % find the intersection of the two data sets, and sort them in ascending order

cl1=adjcls1(idx1); 

cl2=adjcls2(idx2);  

% step one of look-forward bias check
cutoff=60; % number of most recent trading days to cut off 
tday(end-cutoff+1:end, :)=[]; % remove the last cutoff number of days.
cl1(end-cutoff+1:end, :)=[];
cl2(end-cutoff+1:end, :)=[];

trainset=1:252; % define indices for training set

testset=trainset(end)+1:length(tday); % define indices for test set

vnames=strvcat('GLD', 'GDX');

% test for cointegration of GLD and GDX in the trainset.
res=cadf(cl1(trainset), cl2(trainset), 0, 1);

prt(res, vnames);

% determines the hedge ratio on the trainset
results=ols(cl1(trainset), cl2(trainset)); % use regression function 
hedgeRatio=results.beta;

spread=cl1-hedgeRatio*cl2; % spread = GLD - hedgeRatio*GDX

plot(spread(trainset));

figure;

plot(spread(testset));

figure;

spreadMean=mean(spread(trainset)); % mean  of spread on trainset

spreadStd=std(spread(trainset)); % standard deviation of spread on trainset

zscore=(spread - spreadMean)./spreadStd; % z-score of spread

longs=zscore<=-2; % buy spread when its value drops below 2 standard deviations.

shorts=zscore>=2; % short spread when its value rises above 2 standard deviations.

exits=abs(zscore)<=1; % exit any spread position when its value is within 1 standard deviation of its mean.

positions=NaN(length(tday), 2); % initialize positions array

positions(shorts, :)=repmat([-1 1], [length(find(shorts)) 1]); % long entries

positions(longs,  :)=repmat([1 -1], [length(find(longs)) 1]); % short entries

positions(exits,  :)=zeros(length(find(exits)), 2); % exit positions

positions=fillMissingData(positions); % ensure existing positions are carried forward unless there is an exit signal

cl=[cl1 cl2]; % combine the 2 price series

dailyret=(cl - lag1(cl))./lag1(cl);

pnl=sum(lag1(positions).*dailyret, 2);

sharpeTrainset=sqrt(252)*mean(pnl(trainset(2:end)))./std(pnl(trainset(2:end))) % the Sharpe ratio on the training set should be about 2.9

sharpeTestset=sqrt(252)*mean(pnl(testset))./std(pnl(testset)) % the Sharpe ratio on the test set should be about 1.0

plot(cumsum(pnl(testset)));

% step two of look-forward-bias check
oldoutput=load('example3_6_positions');
oldoutput.positions(end-cutoff+1:end, :)=[];
 
if (any(positions~=oldoutput.positions))
    fprintf(1, 'Program has look-forward-bias!\n');
end



