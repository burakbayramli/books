function ylevf = rvarf(y,nlag,w,freq,nfor,begf,sig,tau,theta,x);
% PURPOSE: Estimates a Bayesian autoregressive model of order n
%          using Random-Walk averaging prior and produces f-step-ahead forecasts.
%---------------------------------------------------
% USAGE:   ylevf = rvarf(y,nlag,w,freq,nfor,begf,sig,tau,theta,x)
% where:    y    = an (nobs x neqs) matrix of y-vectors in levels
%           nlag = the lag length
%           w    = an (neqs x neqs) matrix containing prior means
%                  (rows should sum to unity, see below)
%           freq = 1 for annual, 4 for quarterly, 12 for monthly
%           sig  = prior variance hyperparameter (see below)
%           tau  = prior variance hyperparameter (see below)
%          theta = prior variance hyperparameter (see below)
%           x    = an (nobs x nx) matrix of deterministic variables
%                  (in any form, they are not altered during estimation)
%                  (constant term automatically included)
%           nfor = the forecast horizon
%           begf = the beginning date of the forecast
%                             
% priors for important variables:  N(w(i,j),sig) for 1st own lag
%                                  N(  0 ,tau*sig/k) for lag k=2,...,nlag
%               
% priors for unimportant variables are:  N(w(i,j) ,theta*sig/k) for lag k 
%  
% e.g., if y1, y3, y4 are important variables in eq#1, y2 unimportant
%  w(1,1) = 1/3, w(1,3) = 1/3, w(1,4) = 1/3, w(1,2) = 0
%                                              
% typical values would be: sig = .1-.3, tau = 4-8, theta = .5-1  
%
%---------------------------------------------------
% NOTES: - estimation is carried out in annualized growth terms 
%          hence the need for a freq argument input.
%          the prior means rely on common (growth-rate) scaling of variables  
%        - constant term included automatically  
%---------------------------------------------------
% RETURNS:
%  ylevf(1:nfor,1:neqs) = y-forecasts for each equation in levels 
%---------------------------------------------------
% SEE ALSO: varf, bvarf, ecmf, recmf
%---------------------------------------------------
% References: LeSage and Krivelyova (1998) 
% ``A Spatial Prior for Bayesian Vector Autoregressive Models'',
% forthcoming Journal of Regional Science, (on http://www.econ.utoledo.edu)
% and
% LeSage and Krivelova (1997) (on http://www.econ.utoledo.edu)
% ``A Random Walk Averaging Prior for Bayesian Vector Autoregressive Models''

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[nobs neqs] = size(y);
nx = 0;

if nargin == 10 % user is specifying deterministic variables
   [nobs2 nx] = size(x);
elseif nargin == 9 % no deterministic variables
nx = 0;
else
 error('Wrong # of arguments to rvarf');
end;

% adjust nobs to feed the lags
nmin = min(nobs,begf-1);
nobse = nmin - nlag;

% call rvarb to get parameter estimates
if nx ~= 0
bmat = rvarb(y(1:begf-1,:),nlag,w,freq,sig,tau,theta,x(1:begf-1,:));
else
bmat = rvarb(y(1:begf-1,:),nlag,w,freq,sig,tau,theta);
end;

yfor = zeros(nfor,neqs);
ylev = zeros(nfor,neqs);
 
% given bmat values generate future
%  growth rate forecasts 

dy = growthr(y,freq);
   
% 1-step-ahead forecast 
xtrunc = [dy(nmin-(nlag):nmin,:)
          zeros(1,neqs)];
xfor = mlag(xtrunc,nlag);
[xend junk] = size(xfor);
xobs = xfor(xend,:);
if nx > 0
xvec = [xobs x(begf,:) 1];
else
xvec = [xobs 1];
end;

% loop over equations
for i=1:neqs;
bhat = bmat(:,i);
yfor(1,i) = xvec*bhat; % growth rate forecast
end;

xnew = zeros(nlag+1,neqs);

% 2 through nlag-step-ahead forecasts
for step=2:nlag;

if step <= nfor

xnew(1:nlag-step+1,:) = dy(nmin-nlag+step:nmin,:);
xnew(nlag-step+2:nlag,:) = yfor(1:step-1,:);
xnew(nlag+1,:) = zeros(1,neqs);

xfor = mlag(xnew,nlag);
[xend junk] = size(xfor);
xobs = xfor(xend,:);
if nx > 0 
xvec = [xobs x(begf+step-1,:) 1];
else
xvec = [xobs 1];
end;

% loop over equations
for i=1:neqs;
bhat = bmat(:,i);
yfor(step,i) = xvec*bhat;
end;

end;

end;

% nlag through nfore-step-ahead forecasts

for step=nlag:nfor-1;

if step <= nfor;

cnt = step-(nlag-1);

 for i=1:nlag;
  xnew(i,:) = yfor(cnt,:);
  cnt = cnt+1;
 end;
 
xfor = mlag(xnew,nlag);
[xend junk] = size(xfor);
xobs = xfor(xend,:);
if nx > 0
xvec = [xobs x(begf+step,:) 1];
else
xvec = [xobs 1];
end;

% loop over equations
for i=1:neqs;
bhat = bmat(:,i);
yfor(step+1,i) = xvec*bhat;
end;

end; % end of if step
  
end;

% convert growth rate forecasts to levels
ylevf = zeros(nfor,neqs);
yfor = yfor/100;
for step=1:nfor;

if freq < step, % here we can use past level forecasts
   ylevf(step,:) = (1 + yfor(step,:)).*ylevf(step-freq,:);
else % case of freq > step, use past actual levels
   ylevf(step,:) = (1 + yfor(step,:)).*y(begf+step-freq-1,:);
end; % end of if freq <= step

end; % end of for step loop







