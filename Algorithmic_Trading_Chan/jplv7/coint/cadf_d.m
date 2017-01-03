% PURPOSE: demonstrate the use of cadf()
%          function to test for cointegration
%          among I(1) time-series
% ---------------------------------------------
% usage: cadf_d
% ----------------------------------------------

clear all;
nobs = 100;

   dx =   randn(nobs,2);
   y = cumsum(dx); % non-cointegrated system
                   % with 2 I(1) variables
   nlags = 3;
   
  for i=1:nlags;
   res = cadf(y(:,1),y(:,2),-1,i);
   prt(res);
  end;
  
  % generate a co-integrated system
  
  z = y(:,1) + dx(:,2);
  
  % z and y are co-integrated
  for i=1:nlags;  
   res = cadf(z,y(:,1),-1,i);
    prt(res);
  end;
 