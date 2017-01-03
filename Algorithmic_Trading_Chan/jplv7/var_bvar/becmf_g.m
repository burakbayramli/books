function ylevf = becmf_g(y,nlag,nfor,begf,prior,ndraw,nomit,r);
% PURPOSE: Gibbs sampling forecasts for Bayesian error 
%          correction model using Minnesota-type prior
%          dy = A(L) DY + E, E = N(0,sige*V), 
%          V = diag(v1,v2,...vn), r/vi = ID chi(r)/r, r = Gamma(m,k)
%          c = R A(L) + U, U = N(0,Z), Minnesota prior
%---------------------------------------------------
% USAGE:  yfor = becmf_g(y,nlag,nfor,begf,prior,ndraw,nomit,x,transf)
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
%              r = # of co-integrating relations to use
%                  (optional: this will be determined using
%                  Johansen's trace test at 95%-level if left blank)   
%---------------------------------------------------------------
% NOTES: - constant vector automatically included
%        - x-matrix of exogenous variables not allowed
%        - error correction variables are automatically
%          constructed using output from Johansen's ML-estimator     
%---------------------------------------------------------------      
% RETURNS:
%  yfor = an nfor x neqs matrix of level forecasts for each equation
%---------------------------------------------------------------
% SEE ALSO: bvarf_g, becm_g, recmf_g, rvarf_g
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
% find # observations up to forecast period
nmin = min(nobs,begf-1);

% error checking on input
if ~isstruct(prior)
    error('becmf_g: must supply the prior as a structure variable');
end;

nx = 0;

  if nargin == 8 % user supplied r-value
 % use johansen to determine ec variables
 % decrement r by 1 when calling johansen
 jres = johansen(y(1:nmin,:),0,nlag);
 % recover error correction vectors
 ecvectors = jres.evec;
        index = jres.ind; 
 % construct r-error correction variables
 x = mlag(y(1:nmin,index),1)*ecvectors(:,1:r); 
   [nobs2 nx] = size(x);
  elseif nargin == 7 % we have to determine r-value
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
   error('Wrong # of input arguments to becmf');
  end;



% do error checking here, even though it is redundant since
% becm_g will do the same error checking. BUT, we avoid
% confusing the poor user who will get error messages from
% this routine that she called, rather than becm_g

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
        warning('Tightness less than 0.01 in becmf_g');
        elseif tight > 1.0
        warning('Tightness greater than unity in becmf_g');
        end;
    elseif strcmp(fields{i},'weight')
        weight = prior.weight;       
       [wchk1 wchk2] = size(weight);
       if (wchk1 ~= wchk2) 
       error('non-square weight matrix in becmf_g');
       elseif wchk1 > 1
        if wchk1 ~= neqs
        error('wrong size weight matrix in becmf_g');
        end;
       end;
    elseif strcmp(fields{i},'decay')
        decay = prior.decay;    
        if decay < 0
        error('Negative lag decay in becmf_g');
        end;       
    end;
end;


if nlag < 1
error('Lag length less than 1 in becmf_g');
end;

% truncate to begf-1 for estimation 
ytrunc = y(1:nmin,:);


% call becm_g with input information
if r > 0
result = becm_g(ytrunc,nlag,prior,ndraw,nomit,r);
else
result = becm_g(ytrunc,nlag,prior,ndraw,nomit);
end;

% all we really care about is:
% result(eq).bdraw = bhat draws for equation eq

for j=1:neqs;
b = mean(result(j).bdraw);
bmat(:,j) = b';
end;

% given bmat values generate future forecasts 
% These are 1st-differences, 
% we worry about transforming back to levels later


% transform to 1st difference form
dy = zeros(nmin,neqs);
for i=1:neqs;
dy(:,i) = ytrunc(:,i) - lag(ytrunc(:,i),1);
end;


% 1-step-ahead forecast 
xtrunc = [dy(nmin-nlag:nmin,:)
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
yfor(1,i) = xvec*bhat; % NOTE this is a change forecast
ylev(1,i) = yfor(1,i) + y(nmin-1,i); % this adds the previous level
end;

xnew = zeros(nlag+nx+1,neqs);

% 2 through nlag-step-ahead forecasts
for step=2:nlag;

if step <= nfor;

xnew(1:nlag-step+1,:) = dy(nmin-nlag+step:nmin,:);
xnew(nlag-step+2:nlag,:) = yfor(1:step-1,:);
xnew(nlag+1,:) = zeros(1,neqs);


xfor = mlag(xnew,nlag);
[xend junk] = size(xfor);
xobs = xfor(xend,:);
% construct ec terms based on levels forecast from previous periods
if nx > 0
ecterm = ylev(step-1,index)*ecvectors(:,1:r);
xvec = [xobs ecterm 1];
else
xvec = [xobs 1];
end;


% loop over equations
for i=1:neqs;
bhat = bmat(:,i);
yfor(step,i) = xvec*bhat; % change forecast
ylev(step,i) = yfor(step,i) + ylev(step-1,i); % level forecast
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
% construct ec terms based on levels forecast from previous periods
if nx > 0
ecterm = ylev(step,index)*ecvectors(:,1:r);
xvec = [xobs ecterm 1];
else
xvec = [xobs 1];
end;


% loop over equations
for i=1:neqs;
bhat = bmat(:,i);
yfor(step+1,i) = xvec*bhat; % change forecast
ylev(step+1,i) = yfor(step+1,i) + ylev(step-1,i); % level forecast
end;

end;

end;
  
% convert 1st difference forecasts to levels
ylevf = zeros(nfor,neqs);
% 1-step-ahead forecast
ylevf(1,:) = yfor(1,:) + y(begf-1,:); % add change to actual from time t;
% 2-nfor-step-ahead forecasts
for i=2:nfor % 
ylevf(i,:) = yfor(i,:) + ylevf(i-1,:);
end;


