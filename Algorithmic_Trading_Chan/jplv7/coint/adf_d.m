% PURPOSE: demonstrate the use of adf()
%          function to test for unit roots in
%          a levels series
% ---------------------------------------------
% usage: adf_d
% ----------------------------------------------

clear all;
nobs = 100;

   dx =   randn(nobs,1);
   y = cumsum(dx);
   e = randn(nobs,1);
   
   nlags = 3;
  for i=1:nlags;
   res = adf(y,0,i);
   prt(res);
  end;
  
  

