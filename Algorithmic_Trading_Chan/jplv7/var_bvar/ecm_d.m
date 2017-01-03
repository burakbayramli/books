% PURPOSE: demonstrate the use of ecm()
%          function to estimate an error correction model
% ---------------------------------------------
% usage: ecm_d
% ----------------------------------------------

clear all;

load test.dat; % a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5

vnames =  ['  il',
           '  in',    
           '  ky',    
           '  mi',    
           '  oh',    
           '  pa',    
           '  tn',    
           '  wv'];    
     
y = test;
[nobs neqs] = size(y);

nlag = 9;  % number of lags in var-model

% estimate the model
% let routine determine # of co-integrating vectors
result = ecm(y,nlag);

% print results to a file
fid = fopen('ecm.out','wr');
prt_var(result,vnames,fid);
fclose(fid);

disp('Output is in file ecm.out in current directory');



