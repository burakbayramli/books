% PURPOSE: An example of using pftest
%          to print VAR model F-test
%          for Granger-causality                                               
%---------------------------------------------------
% USAGE: pftest_d
%---------------------------------------------------

load test.dat; % a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5
y = test;
     
nlag = 2;  % number of lags in var-model

% estimate the model
results = var(y,nlag);

vnames =  ['illinois     ',
           'indiana      ',    
           'kentucky     ',    
           'michigan     ',    
           'ohio         ',    
           'pennsylvania ',    
           'tennessee    ',    
           'west virginia'];
     

pftest(results,vnames);