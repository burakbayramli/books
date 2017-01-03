% PURPOSE: An example of using recmf(), 
%          to produce ecm-model forecasts                                                 
%          (based on Bayesian Spatial contiguity prior) 
%              
% References: LeSage and Krivelyova (1998) 
% ``A Spatial Prior for Bayesian Vector Autoregressive Models'',
% forthcoming Journal of Regional Science, (on http://www.econ.utoledo.edu)
% and
% LeSage and Krivelova (1997) (on http://www.econ.utoledo.edu)
% ``A Random Walk Averaging Prior for Bayesian Vector Autoregressive Models''            
%---------------------------------------------------
% USAGE: recmf_d
%---------------------------------------------------

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

nfor = 12; % number of forecast periods
nlag = 9;  % number of lags in var-model
begf = nobs-nfor+1; % beginning forecast period

% prior hyperparameters
% priors for contiguous variables:  N(w(i,j),sig) for 1st own lag
%                                  N(  0 ,tau*sig/k) for lag k=2,...,nlag
%               
% priors for non-contiguous variables are:  N(w(i,j) ,theta*sig/k) for lag k 
%  
% e.g., if y1, y3, y4 are contiguous variables in eq#1, y2 non-contiguous
%  w(1,1) = 1/3, w(1,3) = 1/3, w(1,4) = 1/3, w(1,2) = 0
%                                              
% typical values would be: sig = .1-.3, tau = 4-8, theta = .5-1  
sig = 0.5;
tau = 6;
theta = 0.75;
freq = 12;   % monthly data

% this is an example of using 1st-order contiguity
% of the states as weights to produce prior means

 weight = ones(neqs,neqs); % set everything to contiguous
 
 weight(1,4) = 0.0;  % specify non-contiguous states
 weight(1,5) = 0.0;
 weight(1,6) = 0.0;
 weight(1,8) = 0.0;

 weight(2,6) = 0.0;
 weight(2,7) = 0.0;
 weight(2,8) = 0.0;

 weight(3,4) = 0.0;
 weight(3,6) = 0.0;

 weight(4,1) = 0.0;
 weight(4,3) = 0.0;
 weight(4,6) = 0.0;
 weight(4,7) = 0.0;
 weight(4,8) = 0.0;

 weight(5,1) = 0.0;
 weight(5,7) = 0.0;

 weight(6,1) = 0.0;
 weight(6,2) = 0.0;
 weight(6,3) = 0.0;
 weight(6,4) = 0.0;
 weight(6,7) = 0.0;

 weight(7,2) = 0.0;
 weight(7,4) = 0.0;
 weight(7,5) = 0.0;
 weight(7,6) = 0.0;
 weight(7,8) = 0.0;

 weight(8,1) = 0.0;
 weight(8,2) = 0.0;
 weight(8,4) = 0.0;

for ii=1:neqs;
weight(ii,ii) = 0.0; % set main-diagonal to zero
end;


 for ii=1:neqs; % normalize row-sums to unity
 rsum = sum(weight(ii,:));
  for jj=1:neqs;
  weight(ii,jj) = weight(ii,jj)/rsum;
  end;
 end;


% produce historical forecasts
% let routine determine # of co-integrating vectors
fcasts = recmf(y,nlag,weight,freq,nfor,begf,sig,tau,theta);

actual = y(begf:begf+nfor-1,:);

fprintf(1,'actual levels of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(1982,1,12,begf+i-1));
for j=1:neqs;
fprintf(1,'%8.2f ',actual(i,j));
end;
fprintf(1,'\n');
end;

fprintf(1,'RECM model \n');
fprintf(1,'levels forecasts of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(1982,1,12,begf+i-1));
for j=1:neqs;
fprintf(1,'%8.2f ',fcasts(i,j));
end;
fprintf(1,'\n');
end;


% future forecast
begf = nobs+1;

% let the routine determine # of co-integrating vectors
fcasts = recmf(y,nlag,weight,freq,nfor,begf,sig,tau,theta);

fprintf(1,'RECM model \n');
fprintf(1,'FUTURE levels forecast of mining employment \n');
for i=1:nfor
fprintf(1,'%12s ',tsdate(1982,1,12,begf+i-1));
for j=1:neqs;
fprintf(1,'%8.2f ',fcasts(i,j));
end;
fprintf(1,'\n');
end;


