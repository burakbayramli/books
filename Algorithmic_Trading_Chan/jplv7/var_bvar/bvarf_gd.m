% PURPOSE: An example of using bvarf_g(), 
%          Gibbs estimates and forecast using
%          a vector autoregressive model                                                 
%          (with Minnesota prior)                    
%---------------------------------------------------
% USAGE: bvarf_gd
%---------------------------------------------------

load test.dat; % a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5
dates = cal(1982,1,12);

% vnames = strvcat('il','in','ky','mi','oh','pa','tn','wv');

y = test(:,1:2); % use only two variables

vnames =  ['  il',
           '  in'];    
 
[nobs neqs] = size(y);

nlag = 2;  % number of lags in var-model
tight = 0.1;
decay = 0.1;
weight = 0.5; % symmetric weights

% this is an example of using 1st-order contiguity
% of the states as weights as in LeSage and Pan (1995)
% `Using Spatial Contiguity as Bayesian Prior Information 
% in Regional Forecasting Models'' International Regional 
% Science Review, Volume 18, no. 1, pp. 33-53, 1995.

w = [1.0  1.0  1.0  0.1  0.1  0.1  0.1  0.1 
     1.0  1.0  1.0  1.0  1.0  0.1  0.1  0.1 
     1.0  1.0  1.0  0.1  1.0  0.1  1.0  1.0 
     0.1  1.0  0.1  1.0  1.0  0.1  0.1  0.1 
     0.1  1.0  1.0  1.0  1.0  1.0  0.1  1.0 
     0.1  0.1  0.1  0.1  1.0  1.0  0.1  1.0 
     0.1  0.1  1.0  0.1  0.1  0.1  1.0  0.1 
     0.1  0.1  1.0  0.1  1.0  1.0  0.1  1.0];

% set up prior structure
prior.tight = tight;
prior.decay = decay;
prior.weight = weight;
prior.rval = 50;  % homoscedastic prior
% prior.rval = 4; % heteroscedastic prior
ndraw = 1100;
nomit = 100;
begf = ical(1995,1,dates);  % beginning forecast date
nfor = 12;                  % # of forecasts
endf = ical(1995,12,dates); % end forecast dates

% straight bvar model in levels
yfor1 = bvarf(y,nlag,nfor,begf,tight,weight,decay,[],dates);

% estimate the model
yfor2 = bvarf_g(y,nlag,nfor,begf,prior,ndraw,nomit,[],dates);

rnames = 'Dates';
for i=begf:endf
rnames = strvcat(rnames,tsdate(dates,i));
end;
in.rnames = rnames;
in.fmt = '%9.3f';
in.cnames = vnames;
fprintf(1,'Bvar forecasts \n');
mprint(yfor1,in);
fprintf(1,'Bvar Gibbs forecasts \n');
mprint(yfor2,in);




