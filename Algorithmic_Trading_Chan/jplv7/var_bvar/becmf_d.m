% PURPOSE: demonstrate the use of becmf
%          function to forecast a Bayesian
%          error correction model using Minnesota prior
% ---------------------------------------------
% usage: becmf_d
% ----------------------------------------------

load test.dat; % a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5
dates = cal(1982,1,12); % set up calendar structure
y = test;
vnames =  ['il',
           'in',    
           'ky',    
           'mi',    
           'oh',    
           'pa',    
           'tn',    
           'wv'];    
     
[nobs neqs] = size(y);

tight = 0.1;        % Litterman's overall tightness
decay = 0.1;
weight = 0.5;       % symmetric weights          
nlag = 9;

nfor = 12; % number of forecast periods
begf = ical(1995,6,dates); % begin forecast in 1995,6
endf = ical(1996,5,dates); % end forecast in 1996,5

actual = y(begf:begf+nfor-1,:);

fprintf(1,'actual levels of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(dates,begf+i-1));
for j=1:neqs;
fprintf(1,'%8.2f ',actual(i,j));
end;
fprintf(1,'\n');
end;


% determines # of co-integrating relations automatically
fcasts = becmf(y,nlag,nfor,begf,tight,weight,decay);

fprintf(1,'BECM model with co-integrating relations  \n');
fprintf(1,'automatically determined \n');
fprintf(1,'levels forecast of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(dates,begf+i-1));
for j=1:neqs;
fprintf(1,'%8.2f ',fcasts(i,j));
end;
fprintf(1,'\n');
end;

% force the use of only 1 co-integrating relation
fcasts = becmf(y,nlag,nfor,begf,tight,weight,decay,1);

fprintf(1,'BECM model with only 1 co-integrating relation  \n');
fprintf(1,'levels forecast of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(dates,begf+i-1));
for j=1:neqs;
fprintf(1,'%8.2f ',fcasts(i,j));
end;
fprintf(1,'\n');
end;

% future forecast
begf = endf+1;
fcasts = becmf(y,nlag,nfor,begf,tight,weight,decay);

fprintf(1,'BECM model co-integrating relations automatically determined  \n');
fprintf(1,'FUTURE levels forecast of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(dates,begf+i-1));
for j=1:neqs;
fprintf(1,'%8.2f ',fcasts(i,j));
end;
fprintf(1,'\n');
end;

