% PURPOSE: An example of using irf
%          impulse response functions
%                                                           
%---------------------------------------------------
% USAGE: irf_d2
%---------------------------------------------------

clear all;
load test.dat; % a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5
neqs = 8;
y = test(:,1:neqs); % use neqs states
yy = [y log(y)];     
nlag = 3;  % number of lags in var-model

% estimate the model
results = vare(yy,nlag);
vnames1 =  strvcat('illinois','indiana','kentucky','michigan','ohio', ...
                  'pennsylvania','tennessee','west virginia'); 
vnames2 =  strvcat('log(ill)','log(ind)','log(ky)','log(mi)','log(oh)', ...
                  'log(pa)','log(tn)','log(wv)');       
vnames = strvcat(vnames1,vnames2);      
      
nperiods = 48;

[m1 m2] = irf(results,nperiods,'o1',vnames);



