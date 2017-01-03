% PURPOSE: demonstrate the use of becm
%          function to estimate a Bayesian
%          error correction model using Minnesota prior
% ---------------------------------------------
% usage: becm_d
% ----------------------------------------------

load test.dat; % a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5

vnames =  ['il',
           'in',    
           'ky',    
           'mi',    
           'oh',    
           'pa',    
           'tn',    
           'wv'];    
     
y = test;

[nobs neqs] = size(y);

tight = 0.1;
decay = 0.1;
weight = 0.5;
nlag = 9;
      
   % Estimate using becm
   % let the routine determine co-integrating vectors
   result = becm(y,nlag,tight,weight,decay) ;

   % print output to a file
   % fid = fopen('becm.out','wr');
   fid = 1;
   prt_var(result,vnames,fid);
   
