clear;

lookback=20;

% get the files from my dropbox (see the readme), and change
% the base paths below
load('/home/burak/Dropbox/Public/data/inputData_AUDCAD_20120426',  'hhmm', 'tday',  'cl');
idx=find(hhmm==1659);
dailyCl=cl(idx);
tday=tday(idx);

% Annualized interest rates in percent, updated monthly
aud=load('AUD_interestRate', 'yyyy', 'mm', 'rates'); 
cad=load('CAD_interestRate', 'yyyy', 'mm', 'rates');

aud_dailyRates=zeros(size(tday));
for i=1:length(aud.mm)
    idx=find(aud.mm(i)==month(num2str(tday), 'yyyymmdd') & aud.yyyy(i)==year(num2str(tday), 'yyyymmdd'));
    if (~isempty(idx))
        aud_dailyRates(idx)=aud.rates(i);
    end
end
aud_dailyRates=aud_dailyRates/365/100;
% Triple rollover interest on Wednesdays for AUD
isWednesday=weekday(datenum(num2str(tday), 'yyyymmdd'))==4;
aud_dailyRates(isWednesday)=3*aud_dailyRates(isWednesday);

cad_dailyRates=zeros(size(tday));
for i=1:length(cad.mm)
    idx=find(cad.mm(i)==month(num2str(tday), 'yyyymmdd') & cad.yyyy(i)==year(num2str(tday), 'yyyymmdd'));
    if (~isempty(idx))
        cad_dailyRates(idx)=cad.rates(i);
    end
end
cad_dailyRates=cad_dailyRates/365/100;
% Triple rollover interest on Thursdays for CAD
isThursday=weekday(datenum(num2str(tday), 'yyyymmdd'))==5;
cad_dailyRates(isThursday)=3*cad_dailyRates(isThursday);

ma=movingAvg(dailyCl, lookback);
mstd=movingStd(dailyCl, lookback);
z=(dailyCl-ma)./mstd;

% Unlevered return of a linear mean-reverting strategy.
ret=lag(-sign(z), 1).*(log(dailyCl)+lag(-log(dailyCl)+log(1+aud_dailyRates)-log(1+cad_dailyRates), 1));
% ret=lag(-sign(z), 1).*(log(dailyCl)+lag(-log(dailyCl), 1));
ret(isnan(ret))=0;

plot(cumprod(1+ret)-1); % Cumulative compounded return

fprintf(1, 'APR=%f Sharpe=%f\n', prod(1+ret).^(252/length(ret))-1, sqrt(252)*mean(ret)/std(ret));
% APR=0.061564 Sharpe=0.541802

