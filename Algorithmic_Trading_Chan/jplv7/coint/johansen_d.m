% PURPOSE: demonstrate the use of johansen()
%          function to test for cointegration
%          among I(1) time-series
% ---------------------------------------------
% usage: johansen_d
% ----------------------------------------------

clear all;

vnames  = ['  illinos      ',
           '  indiana      ',    
           '  kentucky     ',    
           '  michigan     ',    
           '  ohio         ',    
           '  pennsyvlania ',    
           '  tennesse     ',    
           '  west virginia'];    

load test.dat;
% a sample of monthly employment for 8 states
% in mining from 1982,1 to 1996,5

y = test; % use all eight states
nlag = 9;
cterm = 0;
result = johansen(y,cterm,nlag);

% print results with variable names
prt_coint(result,vnames);

% print results with generic names
%prt(result);