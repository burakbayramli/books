% PURPOSE: An example of using vare, pgranger, prt_var,plt_var
%          to estimate a vector autoregressive model
%          report Granger-causality results, print and plot                                                 
%---------------------------------------------------
% USAGE: vare_d
%---------------------------------------------------

load test.dat; % a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5
y = test;
     
nlag = 2;  % number of lags in var-model

% estimate the model
results = vare(y,nlag);

vnames =  ['illinois     ',
           'indiana      ',    
           'kentucky     ',    
           'michigan     ',    
           'ohio         ',    
           'pennsylvania ',    
           'tennessee    ',    
           'west virginia'];
     

prt(results,vnames);
cutoff = 0.1;
pgranger(results,vnames,cutoff);
plt(results,vnames);

