function ylevf = bvarf(y,nlag,nfor,begf,tight,weight,decay,x,transf);
% PURPOSE: Estimates a Bayesian vector autoregression of order n
%          and produces f-step-ahead forecasts (Minnesota prior)
%---------------------------------------------------------------
% USAGE: yfor = bvarf(y,nlag,nfor,begf,tight,weight,decay,x,transf)
% where:    y    = an (nobs x neqs) matrix of y-vectors in levels
%           nlag = the lag length
%           nfor = the forecast horizon
%           begf = the beginning date of the forecast
%          tight = Litterman's tightness hyperparameter
%         weight = Litterman's symmetric weight (scalar)
%          decay = Litterman's lag decay = lag^(-decay) 
%           x    = an optional matrix of deterministic variables
%         transf = 0, no data transformation
%                = 1, 1st differences used to estimate the model
%                = freq, seasonal differences used to estimate
%                = cal-structure growth rates used to estimate
%                  e.g., cal(1982,1,12) [see cal() function]    
%---------------------------------------------------------------
% NOTE: - use bvarf(y,nlag,nfor,begf,tight,weight,decay,[],transf)
%         for a transformation model with no x's (deterministic variables)
%       - includes constant term automatically
%---------------------------------------------------------------
% RETURNS:
%  yfor = an nfor x neqs matrix of level forecasts for each equation
%---------------------------------------------------------------
% SEE ALSO: bvar, plt_var, prt_var
%---------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin == 9 % user wants us to transform the data
[nobs2 nx] = size(x);
 if isstruct(transf) % a growth rates transform
   tform = 2;
   freq = transf.freq;
   elseif transf == 0  % no transform
   tform = 0;
   freq = 0;
   elseif transf == 1  % 1st difference transform
   tform = 1;
   freq = 0;
   elseif (transf == 1) | (transf == 4) | (transf == 12)
   tform = 3;          % seasonal differences transform
   freq = transf;
   end;
elseif nargin == 8
[nobs2 nx] = size(x);
tform = 0;
freq = 0;
elseif nargin == 7
nx = 0;
tform = 0;
freq = 0;
else
error('Wrong # of arguments to bvarf');
end;

% flag an error where x-variables exist but not enough forecast values
% are supplied for these variables
if nx > 0
   if nobs2 < begf-1+nfor
   error('bvarf: not enough observations in x to forecast');
   end;
end;

[nobs neqs] = size(y);
% adjust nobs to feed the lags
nmin = min(nobs,begf-1);

% error checking on inputs
if nlag < 1
error('Lag length less than 1 in bvarf');
end;

if nlag > nobs
error('Lag length exceeds observations in bvarf');
end;

if tight < 0.01
warning('Tightness less than 0.01 in bvarf');
end;

if tight > 1.0
warning('Tightness greater than unity in bvarf');
end;

if decay < 0
error('Negative lag decay in bvarf');
end;

[wchk1 wchk2] = size(weight);
if (wchk1 ~= wchk2) 
 error('non-square weight matrix in bvarf');
elseif wchk1 > 1
 if wchk1 ~= neqs
 error('wrong size weight matrix in bvarf');
 end;
end;

% check for zeros in weight matrix
if wchk1 == 1
  if weight == 0
  error('bvarf: must have weight > 0');
  end;
elseif wchk1 > 1
  zip = find(weight == 0);
 if length(zip) ~= 0
 error('bvarf: must have weights > 0');
 end;
end;


% nvar adjusted for constant term and deterministic variables
k = neqs*nlag + nx + 1;
ndiff = 0;

switch tform

case 1 % 1st differences transform

% transform data
dy = y - mlag(y,1);
ndiff = 1;
% generate lagged rhs matrix
xlag = mlag(dy,nlag);
% constant term
iota = ones(nobs,1);
% truncate variables to feed lags and 1st diff and end at begf-1
iota = trimr(iota,nlag+1,nobs-begf+1);
dys =  trimr(dy,nlag+1,nobs-begf+1);
xlag = trimr(xlag,nlag+1,nobs-begf+1);

% add x-matrix and constant to x-matrix
if nx > 0
xmat = [xlag x(nlag+2:nmin,:) iota];
else
xmat = [xlag iota];
end;

% end of 1st difference transformation case

case 2 % growth rates transformation

% transform data
dy = growthr(y,freq);
% generate lagged rhs matrix
xlag = mlag(dy,nlag);
% constant term
iota = ones(nobs,1);
% truncate variables to feed lags and freq diff's and end at begf-1
iota = trimr(iota,nlag+freq,nobs-begf+1);
dys = trimr(dy,nlag+freq,nobs-begf+1);
xlag = trimr(xlag,nlag+freq,nobs-begf+1);

% add x-matrix and constant to x-matrix
if nx > 0
xmat = [xlag x(nlag+freq+1:nmin,:) iota];
else
xmat = [xlag iota];
end;

% end of growth-rates transform case

case 3 % seasonal differences transform

% transform data
dy = y - lag(y,freq);
% generate lagged rhs matrix
xlag = mlag(dy,nlag);
% constant term
iota = ones(nobs,1);
% truncate variables to feed lags and freq diff's and end at begf-1
iota = trimr(iota,nlag+freq,nobs-begf+1);
dys = trimr(dy,nlag+freq,nobs-begf+1);
xlag = trimr(xlag,nlag+freq,nobs-begf+1);

% add x-matrix and constant to x-matrix
if nx > 0
xmat = [xlag x(nlag+freq+1:nmin,:) iota];
else
xmat = [xlag iota];
end;

otherwise  % case of no transformation

% generate lagged rhs matrix
xlag = mlag(y,nlag);
% constant term
iota = ones(nobs,1);
% truncate to feed lags and to end at begf-1 for estimation
dys  = trimr(y,nlag,nobs-begf+1);
dy   = y;
xlag = trimr(xlag,nlag,nobs-begf+1);
iota = trimr(iota,nlag,nobs-begf+1);

% add x-matrix and constant to x-matrix
if nx > 0
xmat = [xlag x(nlag+1:nmin,:) iota];
else
xmat = [xlag iota];
end;

end; % end of data transformation cases

% do scaling here 
% determine scale factors using univariate AR model
% Doan uses the full vector whereas we truncate the
% first lags, so we will get slightly difference estimates

scale = zeros(neqs,1);
scale2 = zeros(neqs,neqs);
ytmp = zeros(nmin,1);

for j=1:neqs
 ytmp = dy(freq+ndiff+1:nmin,j);
 scale(j,1) = scstd(ytmp,length(ytmp),nlag);
end;

for j=1:neqs;
 for i=1:neqs;
 scale2(i,j) = scale(j)/scale(i);
 end;
end;

% form xpx only once to save time
xpx = xmat'*xmat;

% pull out each y-vector and run regressions
for j=1:neqs;

yvec = dy(nlag+freq+ndiff+1:nmin,j);
xpy = xmat'*yvec;

reslt = theilbf(xpy,xpx,nlag,neqs,j,tight,weight,decay,scale2,scale,nx);

bmat(:,j) = reslt.beta;

end;

% given bmat values generate future forecasts 
% These may be levels, 1st-differences, growth rates or seas diff's
% we worry transforming back to levels later
    
% 1-step-ahead forecast 
xtrunc = [dy(nmin-nlag:nmin,:)
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
yfor(1,i) = xvec*bhat;
end;

xnew = zeros(nlag+1,neqs);

% 2 through nlag-step-ahead forecasts
for step=2:nlag;

if step <= nfor;

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

end;

end;

  
% we now worry about transforming the forecasts back
% to levels

switch tform

case 1 % 1st differences forecasts
% convert 1st difference forecasts to levels
ylevf = zeros(nfor,neqs);
% 1-step-ahead forecast
ylevf(1,:) = yfor(1,:) + y(begf-1,:); % add change to actual from time t;
% 2-nfor-step-ahead forecasts
for i=2:nfor % 
ylevf(i,:) = yfor(i,:) + ylevf(i-1,:);
end;

% end of 1st differences case

case 2 % growth rates forecasts
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

case 3 % seasonal difference forecasts
% convert seasonal difference forecasts to levels

for step=1:nfor;

if freq < step, % here we use past level forecasts
   ylevf(step,:) = yfor(step,:) + ylevf(step-freq,:);
else % case of freq > step, use past actual levels
   ylevf(step,:) = yfor(step,:) + y(begf+step-freq-1,:);
end; % end of if freq <= step

end; % end of for step loop

otherwise % no transformation, so we have level forecasts already
ylevf = yfor;

end;


