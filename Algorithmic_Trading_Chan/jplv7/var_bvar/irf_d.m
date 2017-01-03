% PURPOSE: An example of using irf
%          impulse response functions
%                                                           
%---------------------------------------------------
% USAGE: irf_d
%---------------------------------------------------

load test.dat; % a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5
neqs = 8;
y = test(:,1:neqs); % use neqs states
     
nlag = 6;  % number of lags in var-model

% estimate the model
results = vare(y,nlag);
vnames =  strvcat('illinois','indiana','kentucky','michigan','ohio', ...
                  'pennsylvania','tennessee','west virginia'); 
      
nperiods = 48;

[m1 m2] = irf(results,nperiods,'o1',vnames);



