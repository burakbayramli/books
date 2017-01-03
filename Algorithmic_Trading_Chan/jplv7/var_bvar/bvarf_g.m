function ylevf = bvarf_g(y,nlag,nfor,begf,prior,ndraw,nomit,x,transf);
% PURPOSE: Gibbs sampling forecasts for Bayesian vector 
%          autoregressive model using Minnesota-type prior
%          y = A(L) Y + X B + E, E = N(0,sige*V), 
%          V = diag(v1,v2,...vn), r/vi = ID chi(r)/r, r = Gamma(m,k)
%          c = R A(L) + U, U = N(0,Z), Minnesota prior
%          diffuse prior on B is used
%---------------------------------------------------
% USAGE:  yfor = bvarf_g(y,nlag,nfor,begf,prior,ndraw,nomit,x,transf)
% where:    y    = an (nobs x neqs) matrix of y-vectors
%           nlag = the lag length
%           nfor = the forecast horizon
%           begf = the beginning date of the forecast           
%          prior = a structure variable
%               prior.tight,  Litterman's tightness hyperparameter
%               prior.weight, Litterman's weight (matrix or scalar)
%               prior.decay,  Litterman's lag decay = lag^(-decay) 
%               prior.rval, r prior hyperparameter, default=4
%               prior.m,    informative Gamma(m,k) prior on r
%               prior.k,    informative Gamma(m,k) prior on r  
%          ndraw = # of draws
%          nomit = # of initial draws omitted for burn-in       
%          x     = an optional (nobs x nx) matrix of variables
%         transf = 0, no data transformation
%                = 1, 1st differences used to estimate the model
%                = freq, seasonal differences used to estimate
%                = cal-structure growth rates used to estimate
%                  e.g., cal(1982,1,12) [see cal() function]   
%---------------------------------------------------------------
% NOTE: - use bvarf_g(y,nlag,nfor,begf,prior,ndraw,nomit,[],transf)
%         for a transformation model with no x's (deterministic variables)
%       - includes constant term automatically
%---------------------------------------------------------------      
% RETURNS:
%  yfor = an nfor x neqs matrix of level forecasts for each equation
%---------------------------------------------------------------
% SEE ALSO: bvar_g, becmf_g, recmf_g, rvarf_g
%---------------------------------------------------------------                      
% REFERENCES:  LeSage, J.P. Applied Econometrics using MATLAB
%---------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[nobs neqs] = size(y);

% error checking on input
if ~isstruct(prior)
    error('bvarf_g: must supply the prior as a structure variable');

elseif nargin == 9 % user wants us to transform the data
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
[nobs2 nx] = size(x)
tform = 0;
freq = 0;
elseif nargin == 7
nx = 0;
tform = 0;
freq = 0;
else
error('Wrong # of arguments to bvarf_g');
end;
    
fields = fieldnames(prior);
nf = length(fields);
mm = 0; rval = 4; % rval = 4 is default
nu = 0; d0 = 0; % default to a diffuse prior on sige
for i=1:nf
    if strcmp(fields{i},'rval')
        rval = prior.rval; 
    elseif strcmp(fields{i},'m')
        mm = prior.m;
        kk = prior.k;
        rval = gamm_rnd(1,1,mm,kk);    % initial value for rval
    elseif strcmp(fields{i},'tight')
        tight = prior.tight;
        if tight < 0.01
        warning('Tightness less than 0.01 in bvarf_g');
        elseif tight > 1.0
        warning('Tightness greater than unity in bvarf_g');
        end;
    elseif strcmp(fields{i},'weight')
        weight = prior.weight;       
       [wchk1 wchk2] = size(weight);
       if (wchk1 ~= wchk2) 
       error('non-square weight matrix in bvarf_g');
       elseif wchk1 > 1
        if wchk1 ~= neqs
        error('wrong size weight matrix in bvarf_g');
        end;
       end;
    elseif strcmp(fields{i},'decay')
        decay = prior.decay;    
        if decay < 0
        error('Negative lag decay in bvarf_g');
        end;       
    end;
end;


if nlag < 1
error('Lag length less than 1 in bvarf_g');
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

% nvar adjusted for constant term and deterministic variables
k = neqs*nlag + nx + 1;
ndiff = 0;

% adjust nobs to feed the lags
nobse = nobs - nlag;

% nvar adjusted for constant term
k = neqs*nlag + 1 + nx;
nvar = k;

switch tform

case 1 % 1st differences transform
% transform data
dy = y - mlag(y,1);

case 2 % growth rates transformation
% transform data
dy = growthr(y,freq);

case 3 % seasonal differences transform
% transform data
dy = y - lag(y,freq);

otherwise  % case of no transformation
dy   = y;

end; % end of data transformation cases

% truncate to account for transformation
for j=1:neqs;
yvec(:,j) = dy(nlag+freq+ndiff+1:nmin,j);
end;

% call bvar_g with transformed data in dy(1:nmin,:) and prior information

if nx > 0
result = bvar_g(yvec,nlag,ndraw,nomit,prior,x);
else
result = bvar_g(yvec,nlag,ndraw,nomit,prior);
end;

% all we really care about is:
% results(eq).bdraw = bhat draws for equation eq

for j=1:neqs;
b = mean(result(j).bdraw);
bmat(:,j) = b';
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

