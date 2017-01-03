% PURPOSE: demonstrate the use of lrratio()
%          function to determine optimal lag length of a VAR
% ---------------------------------------------
% usage: lrratio_d
% ----------------------------------------------

load test.dat;% a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5

y = test;
% will print results to the MATLAB command window
% set flag for Sim's correction factor
sims = 1;
maxlag = 12;
minlag = 3;
lrratio(y,maxlag,minlag,sims);
