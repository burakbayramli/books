% PURPOSE: An example of using arf(),                               
%          to produce ar-model forecasts                    
%---------------------------------------------------
% USAGE: arf_d
%---------------------------------------------------

load test.dat; % a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5

vnames =  strvcat('il');
     
y = test(:,1); % illinois employment
[nobs junk] = size(y);

nfor = 12; % number of forecast periods
nlag = 6;  % number of lags in var-model
begf = nobs-nfor+1; % beginning forecast period

% no data transformation example
fcasts = arf(y,nlag,nfor,begf);

actual = y(begf:begf+nfor-1,1);

fprintf(1,'actual mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(1982,1,12,begf+i-1));
fprintf(1,'%8.2f ',actual(i,1));
fprintf(1,'\n');
end;

fprintf(1,'AR model in levels estimated \n');
fprintf(1,'forecast of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(1982,1,12,begf+i-1));
fprintf(1,'%8.2f ',fcasts(i,1));
fprintf(1,'\n');
end;

% seasonal differences data transformation example
freq = 12; % set frequency of the data to monthly
fcasts = arf(y,nlag,nfor,begf,freq);

fprintf(1,'AR model with seasonally differenced data estimated \n');
fprintf(1,'forecast of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(1982,1,12,begf+i-1));
fprintf(1,'%8.2f ',fcasts(i,1));
fprintf(1,'\n');
end;

% 1st differences data transformation example
fcasts = arf(y,nlag,nfor,begf,1);

fprintf(1,'AR model with 1st differenced data estimated \n');
fprintf(1,'forecast of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(1982,1,12,begf+i-1));
fprintf(1,'%8.2f ',fcasts(i,1));
fprintf(1,'\n');
end;

% growth-rates data transformation example
cstruc = cal(1982,1,12); % set up calendar structure
fcasts = arf(y,nlag,nfor,begf,cstruc);

fprintf(1,'AR model with growth-rates data estimated \n');
fprintf(1,'forecast of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(1982,1,12,begf+i-1));
fprintf(1,'%8.2f ',fcasts(i,1));
fprintf(1,'\n');
end;

