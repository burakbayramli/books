% PURPOSE: demonstrate the use of phillips()
%          function to test for cointegration
%          among I(1) time-series
% ---------------------------------------------
% usage: phillips_d
% ----------------------------------------------

clear all;

nobs = 50;

   dx =   randn(nobs,2);
   y = cumsum(dx); % non-cointegrated system
                   % with 2 I(1) variables
   nlags = 1;
   
info.cnames = strvcat('Phillips stat','signif');
disp('****************************************');
disp('Case of no cointegration');
disp('****************************************');

  for i=0:nlags;
   result = phillips(y(:,1),y(:,2),i);
   prt(result);
  end;

  
  % generate a co-integrated system
  
  z = y(:,1) + dx(:,2);
disp('****************************************');
disp('Case of cointegration');  
disp('****************************************');

  % z and y are co-integrated
  for i=0:nlags;  
   result = phillips(z,y(:,1),i);
    prt(result);
  end;
 