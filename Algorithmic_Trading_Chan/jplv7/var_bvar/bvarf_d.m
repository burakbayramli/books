% PURPOSE: An example of using bvarf(), 
%          to produce bvar-model forecasts                                                 
%          (Minnesota prior)                    
%---------------------------------------------------
% USAGE: bvarf_d
%---------------------------------------------------

load test.dat; % a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5
dates = cal(1982,1,12);
y = test;

vnames =  ['  il',
           '  in',    
           '  ky',    
           '  mi',    
           '  oh',    
           '  pa',    
           '  tn',    
           '  wv'];    
     


nlag = 6;  % number of lags in var-model
begf = ical(1995,6,dates); % beginning forecast period
nfor = 12; % number of forecast periods
endf = cal(dates,begf+nfor-1); % end of forecast period
tight = 0.1;
decay = 0.1;
weight = 0.5; % symmetric weights

% this is an example of using 1st-order contiguity
% of the states as weights as in LeSage and Pan (1995)
% `Using Spatial Contiguity as Bayesian Prior Information 
% in Regional Forecasting Models'' International Regional 
% Science Review, Volume 18, no. 1, pp. 33-53, 1995.

w = [1.0  1.0  1.0  0.1  0.1  0.1  0.1  0.1 
     1.0  1.0  1.0  1.0  1.0  0.1  0.1  0.1 
     1.0  1.0  1.0  0.1  1.0  0.1  1.0  1.0 
     0.1  1.0  0.1  1.0  1.0  0.1  0.1  0.1 
     0.1  1.0  1.0  1.0  1.0  1.0  0.1  1.0 
     0.1  0.1  0.1  0.1  1.0  1.0  0.1  1.0 
     0.1  0.1  1.0  0.1  0.1  0.1  1.0  0.1 
     0.1  0.1  1.0  0.1  1.0  1.0  0.1  1.0];

% no data transformation example
fcasts = bvarf(y,nlag,nfor,begf,tight,weight,decay);

actual = y(begf:begf+nfor-1,:);

fprintf(1,'actual mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(1982,1,12,begf+i-1));
for j=1:neqs;
fprintf(1,'%8.2f ',actual(i,j));
end;
fprintf(1,'\n');
end;

fprintf(1,'BVAR model in levels estimated \n');
fprintf(1,'forecast of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(1982,1,12,begf+i-1));
for j=1:neqs;
fprintf(1,'%8.2f ',fcasts(i,j));
end;
fprintf(1,'\n');
end;

% seasonal differences data transformation example
freq = 12; % set frequency of the data to monthly
fcasts = bvarf(y,nlag,nfor,begf,tight,weight,decay,[],freq);

fprintf(1,'BVAR model with seasonally differenced data estimated \n');
fprintf(1,'forecast of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(1982,1,12,begf+i-1));
for j=1:neqs;
fprintf(1,'%8.2f ',fcasts(i,j));
end;
fprintf(1,'\n');
end;

% 1st differences data transformation example
fcasts = bvarf(y,nlag,nfor,begf,tight,weight,decay,[],1);

fprintf(1,'BVAR model with 1st differenced data estimated \n');
fprintf(1,'forecast of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(1982,1,12,begf+i-1));
for j=1:neqs;
fprintf(1,'%8.2f ',fcasts(i,j));
end;
fprintf(1,'\n');
end;

% growth-rates data transformation example
cstruc = cal(1982,1,12); % set up calendar structure
fcasts = bvarf(y,nlag,nfor,begf,tight,weight,decay,[],cstruc);

fprintf(1,'BVAR model with growth-rates data estimated \n');
fprintf(1,'forecast of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(1982,1,12,begf+i-1));
for j=1:neqs;
fprintf(1,'%8.2f ',fcasts(i,j));
end;
fprintf(1,'\n');
end;

% future forecast
begf = nobs+1;

% 1st differences data transformation example
fcasts = bvarf(y,nlag,nfor,begf,tight,weight,decay,[],1);

fprintf(1,'BVAR model with 1st differenced data estimated \n');
fprintf(1,'FUTURE forecast of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(1982,1,12,begf+i-1));
for j=1:neqs;
fprintf(1,'%8.2f ',fcasts(i,j));
end;
fprintf(1,'\n');
end;


