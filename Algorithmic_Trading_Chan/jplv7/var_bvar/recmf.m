function ylevf = recmf(y,nlag,w,freq,nfor,begf,sig,tau,theta,r);
% PURPOSE: Estimates a Bayesian error correction model of order n
%          using Random-Walk averaging prior and produces f-step-ahead forecasts.          
%---------------------------------------------------
% USAGE:    yfor = recmf(y,nlag,w,freq,nfor,begf,sig,tau,theta,r)
% where:    y    = an (nobs x neqs) matrix of y-vectors in levels
%           nlag = the lag length
%           w    = a weighting for important variables
%           freq = 1 for annual, 4 for quarterly, 12 for monthly
%           sig  = prior variance hyperparameter (see below)
%           tau  = prior variance hyperparameter (see below)
%          theta = prior variance hyperparameter (see below)
%              r = # of co-integrating relations to use
%                  (optional: this will be determined using
%                  Johansen's trace test at 95%-level if left blank)            
%           nfor = the forecast horizon
%           begf = the beginning date of the forecast
% priors for important variables are:  N(1/ci,sig) for 1st own lag 
%                                      (ci = # of important)
%                                      N(  0 ,tau*sig/k) for lag k=2,...,nlag
% priors for unimportant variables are:  N(  0 ,theta*sig/k) for lag k                                             
% typical values would be: sig = .1-.3, tau = 4-8, theta = .5-1  
%---------------------------------------------------
% NOTES: - estimation is carried out in annualized growth terms 
%          hence the need for a freq argument input.
%          the prior means rely on common (growth-rate) scaling of variables  
%        - constant term included automatically 
%        - x-matrix of exogenous variables not allowed
%        - error correction variables are automatically
%          constructed using output from Johansen's ML-estimator  
%---------------------------------------------------
% RETURNS:
%  yfor = (nfor x neqs) matrix of levels forecasts for each equation 
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

% adjust nobs to feed the lags
nmin = min(nobs,begf-1);
nobse = nmin - nlag;

  if nargin == 10 % user supplied r-value
 % use johansen to determine ec variables
 % decrement r by 1 when calling johansen
 jres = johansen(y(1:nmin,:),0,nlag);
 % recover error correction vectors
 ecvectors = jres.evec;
        index = jres.ind; 
 % construct r-error correction variables
 x = mlag(y(1:nmin,index),1)*ecvectors(:,1:r); 
   [nobs2 nx] = size(x);
  elseif nargin == 9 % we have to determine r-value
 jres = johansen(y(1:nmin,:),0,nlag);
 % find r = # significant co-integrating relations using
 % the trace statistic output
 trstat = jres.lr1;
 tsignf = jres.cvt;
 r = 0;
 for i=1:neqs;
  if trstat(i,1) > tsignf(i,2)
   r = i;
  end;
 end;
 % recover error correction vectors
 ecvectors = jres.evec;
        index = jres.ind; 
 % construct r error correction variables
 x = mlag(y(1:nmin,index),1)*ecvectors(:,1:r); 
   [nobs2 nx] = size(x); 
  else
   error('Wrong # of input arguments to recmf');
  end;
 
% adjust nvar for constant term and error correction terms
k = neqs*nlag+nx+1;

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
ylevf = zeros(nfor,neqs); % storage for level forecasts
   
% 1-step-ahead forecast 
xtrunc = [dy(nmin-(nlag):nmin,:)
          zeros(1,neqs)];
xfor = mlag(xtrunc,nlag);
[xend junk] = size(xfor);
xobs = xfor(xend,:);
if nx > 0
ecterm = y(begf-1,index)*ecvectors(:,1:r); % add ec variables 
xvec = [xobs ecterm 1];
else
xvec = [xobs 1];
end;

% loop over equations
for i=1:neqs;
bhat = bmat(:,i);
yfor(1,i) = xvec*bhat/100; % growth rate forecast
ylevf(1,i) = (1+yfor(1,i))*y(begf-freq,i); % construct level forecasts
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
if nx > 0 % add ec variables based on past level forecasts 
   ecterm = ylevf(step-1,index)*ecvectors(:,1:r); 
xvec = [xobs ecterm 1];
else
xvec = [xobs 1];
end;

% loop over equations
for i=1:neqs;
bhat = bmat(:,i);
yfor(step,i) = xvec*bhat/100;
   if freq < step, % here we can use past level forecasts
   ylevf(step,:) = (1 + yfor(step,:)).*ylevf(step-freq,:);
   else % case of freq > step, use past actual levels
   ylevf(step,:) = (1 + yfor(step,:)).*y(begf+step-freq-1,:);
   end; % end of if freq <= step
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
if nx > 0 % add ec variables based on past level forecasts 
   ecterm = ylevf(step,index)*ecvectors(:,1:r); 
xvec = [xobs ecterm 1];
else
xvec = [xobs 1];
end;

% loop over equations
for i=1:neqs;
bhat = bmat(:,i);
yfor(step+1,i) = xvec*bhat/100;
   if freq < step+1, % here we can use past level forecasts
   ylevf(step+1,:) = (1 + yfor(step+1,:)).*ylevf(step+1-freq,:);
   else % case of freq > step, use past actual levels
   ylevf(step+1,:) = (1 + yfor(step+1,:)).*y(begf+step-freq,:);
   end; % end of if freq <= step
end;

end; % end of if step
  
end;









