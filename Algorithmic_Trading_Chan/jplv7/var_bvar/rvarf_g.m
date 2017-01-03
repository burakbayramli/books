function ylevf = rvarf_g(y,nlag,nfor,begf,prior,ndraw,nomit,x);
% PURPOSE: Gibbs forecasts for a Bayesian vector autoregressive 
%          model using the random-walk averaging prior 
%          y = A(L) Y + X B + E, E = N(0,sige*V), 
%          V = diag(v1,v2,...vn), r/vi = ID chi(r)/r, r = Gamma(m,k)
%          c = R A(L) + U, U = N(0,Z), random-walk averaging prior
%          diffuse prior on B is used          
%---------------------------------------------------
% USAGE:  yfor = rvarf_g(y,nlag,nfor,begf,prior,ndraw,nomit,x)
% WHERE:    y    = an (nobs x neqs) matrix of y-vectors (in levels)
%           nlag = the lag length
%           nfor = the forecast horizon
%           begf = the beginning date of the forecast   
%          prior = a structure variable
%               prior.rval, r prior hyperparameter, default=4
%               prior.m,    informative Gamma(m,k) prior on r
%               prior.k,    informative Gamma(m,k) prior on r 
%               prior.w,    an (neqs x neqs) matrix containing prior means
%                           (rows should sum to unity, see below)
%               prior.freq = 1 for annual, 4 for quarterly, 12 for monthly
%               prior.sig  = prior variance hyperparameter (see below)
%               prior.tau  = prior variance hyperparameter (see below)
%               prior.theta = prior variance hyperparameter (see below)                
%          ndraw = # of draws
%          nomit = # of initial draws omitted for burn-in                 
%           x    = an (nobs x nx) matrix of deterministic variables
%                  (in any form, they are not altered during estimation)
%                  (constant term automatically included)                  
% priors for important variables:  N(w(i,j),sig) for 1st own lag
%                                  N(  0 ,tau*sig/k) for lag k=2,...,nlag               
% priors for unimportant variables: N(w(i,j) ,theta*sig/k) for lag 1 
%                                   N(  0 ,theta*sig/k)    for lag k=2,...,nlag  
% e.g., if y1, y3, y4 are important variables in eq#1, y2 unimportant
%  w(1,1) = 1/3, w(1,3) = 1/3, w(1,4) = 1/3, w(1,2) = 0                                              
% typical values would be: sig = .1-.3, tau = 4-8, theta = .5-1  
%---------------------------------------------------
% NOTES: - estimation is carried out in annualized growth terms because 
%          the prior means rely on common (growth-rate) scaling of variables
%          hence the need for a freq argument input.
%        - constant term included automatically  
%---------------------------------------------------
% RETURNS:
%  yfor = an nfor x neqs matrix of level forecasts for each equation
%---------------------------------------------------------------
% ---------------------------------------------------    
% SEE ALSO: bvarf_g, becmf_g, recmf_g, rvar_g
% ---------------------------------------------------
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
% find # observations up to forecast period
nmin = min(nobs,begf-1);

nx = 0;

% error checking on input
if ~isstruct(prior)
    error('rvarf_g: must supply the prior as a structure variable');
end;


if nargin == 8 % user is specifying deterministic variables
   [nobs2 nx] = size(x);
   
elseif nargin == 7 % no deterministic variables
nx = 0;
else
 error('Wrong # of arguments to rvarf_g');
end;

% do error checking here, even though it is redundant since
% rvar_g will do the same error checking. BUT, we avoid
% confusing the poor user who will get error messages from
% this routine that he called, rather than rvar_g

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
    elseif strcmp(fields{i},'tau')
        tau = prior.tau;
    elseif strcmp(fields{i},'w')
        w = prior.w;       
       [wchk1 wchk2] = size(w);
       if (wchk1 ~= wchk2) 
       error('non-square w matrix in rvarf_g');
       elseif wchk1 > 1
        if wchk1 ~= neqs
        error('wrong size w matrix in rvarf_g');
        end;
       end;
    elseif strcmp(fields{i},'theta')
        theta = prior.theta;   
    elseif strcmp(fields{i},'sig')
        sig = prior.sig; 
    elseif strcmp(fields{i},'freq')
        freq = prior.freq;         
    end;
end;

if nlag < 1
error('Lag length less than 1 in rvarf_g');
end;

% truncate to begf-1 for estimation 
ytrunc = y(1:nmin,:);

% call rvar_g with input information
if nx > 0
result = rvar_g(ytrunc,nlag,prior,ndraw,nomit,x);
else
result = rvar_g(ytrunc,nlag,prior,ndraw,nomit);
end;

% all we really care about is:
% result(eq).bdraw = bhat draws for equation eq
for j=1:neqs;
b = mean(result(j).bdraw);
bmat(:,j) = b';
end;

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





