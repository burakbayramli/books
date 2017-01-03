function result = sur(neqs,y,x,iflag,info)
% PURPOSE: computes seemingly unrelated regression estimates
%          for a model with neqs-equations
%---------------------------------------------------
% USAGE:    results = sur(neqs,y,x,iflag,info)
%       or, results = sur(neqs,y,x) (for no iteration)
% where: 
%      neqs  = # of equations
%        y   = an 'eq' structure containing dependent variables 
%              e.g. y(1).eq = y1; y(2).eq = y2; y(3).eq = y3;
%        x   = an 'eq' structure containing explanatory variables
%                 e.g. x(1).eq = [iota x1 x4]; 
%                      x(2).eq = [iota x1]; 
%                      x(3).eq = [iota x1 x2 x5];
%      iflag = 1 for iteration on error covariance matrix, 
%              0 for no iteration (0 = default) 
%       info = a structure for iteration options:
%              info.itmax = maximum # of iterations (default = 100)
%              info.crit  = convergence criterion  for bhat change   
%                           (default = 0.001)                             
%---------------------------------------------------
%        NOTE:  x(i), i=1,...,G should include a constant vector
%               if you want one in the equation
%---------------------------------------------------
% RETURNS a structure:
%   result.meth      = 'sur'
%   result(eq).beta  = bhat for each equation            
%   result(eq).tstat = tstat for each equation            
%   result(eq).tprob = tprobs for each equation        
%   result(eq).resid = residuals for each equation      
%   result(eq).yhat  = yhats for each equation         
%   result(eq).y     = y for each equation             
%   result(eq).rsqr  = r-squared for each equation     
%   result(eq).rbar  = r-squared adj for each equation  
%   result(eq).nvar  = nvar in each equation     
%   result(eq).sige  = e'e/nobs for each equation 
%   result(eq).dw    = Durbin-Watson 
%   result.srsqr     = system-wide R-squared
%   result.nobs      = nobs 
%   result.neqs      = neqs
%   result.sigma     = sig(i,j) across equations
%   result.ccor      = cross-equation correlation matrix
%   result.iter      = # of iterations if iflag = 1, or 0 
%   result.convg     = convergence change in bhat estimates
% --------------------------------------------------
% SEE ALSO: prt(), prt_eqs(), plt()
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% Texas State University-San Marcos
% jlesage@spatial-econometrics.com
%
% updated to fix an error January, 2009
% on line 393 yhat was incorrectly defined as the residual

if nargin == 3
itflag = 0;
icnt = 0;
elseif nargin == 4
itflag = iflag;
itmax = 100;
crit = 0.001;
elseif nargin == 5
 if ~isstruct(info)
 error('sur requires a structure for iteration options');
 end;
itmax = 100;
crit = 0.001;
itflag = 1;
fields = fieldnames(info);
nf = length(fields);
for i=1:nf
    if strcmp(fields{i},'itmax')
        itmax = info.itmax; 
    elseif strcmp(fields{i},'crit')
        crit = info.crit;
    end;
end;

else
error('Wrong # of arguments to sur'); 
end;

result.meth = 'sur';
result.neqs = neqs;

% error checking on input of structures
if ~isstruct(y)
error('sur requires a structure for y as input');
end;

if ~isstruct(x)
error('sur requires a structure for x as input');
end;

% find the # of equations
chk = fieldnames(y);
if (strcmp(chk,'eq') ~= 1)
error('Use eq as the fieldname for y');
end;

chk = fieldnames(x);
if (strcmp(chk,'eq') ~= 1)
error('Use eq as the fieldname for x');
end;

nobs = length(y(1).eq);
result.nobs = nobs;

% error checking that all equations contain the same # of observations
nobsy = zeros(neqs,1);
nobsx = zeros(neqs,1);
for i=1:neqs;
[nobsy(i,1) junk] = size(y(i).eq);
result(i).y = y(i).eq; % fill-in y-vectors
[nobsx(i,1) result(i).nvar] = size(x(i).eq); % fill-in nvar for each equation
end;

tst = find(nobsy ~= nobsx);
if length(tst) ~= 0
error('sur only handles same # obs for all equations');
end;

% fill-in initial sigma matrix using ols
emat = zeros(nobs,neqs);
for i=1:neqs;
emat(:,i) = olse(y(i).eq,x(i).eq);
end;

sigma = zeros(neqs,neqs);
for i=1:neqs;
 for j=i:neqs;
    sigma(i,j) = (emat(:,i)'*emat(:,j))/nobs;
    if j > 1;
  sigma(j,i) = sigma(i,j);
    end;
end;
end;

sigmai = inv(sigma); % find sigma-inverse
% fill-in sig*(x'x) matrix
nx = 0;              % total number of x-variables in all equations
nxi = zeros(neqs,1); % left dimension of xx

for i=1:neqs;
 nx = nx + result(i).nvar;
 nxi(i,1) = result(i).nvar;
end;

nxj = nxi'; % right dimension of xx
xx = zeros(nx,nx);
si = 1;
sj = 1;
for i=1:neqs;
 for j=1:neqs;
  xx(si:si+nxi(i,1)-1,sj:sj+nxj(1,j)-1) = sigmai(i,j)*(x(i).eq'*x(j).eq);
  sj = sj+nxj(1,j);
 end;
 si = si+nxi(i,1);
 sj = 1;
end;

xxi = inv(xx); % find xpx-inverse for inference

% fill-in xpy vector
si = 1;
sj = 1;
xy = zeros(nx,1);

for i=1:neqs;
 xp = x(i).eq';
 for j=1:neqs;
   yt = y(j).eq;
  xy(si:si+nxi(i,1)-1,1) = xy(si:si+nxi(i,1)-1,1) + sigmai(i,j)*xp*yt;
   end;
 si = si+nxi(i,1);
end;

switch itflag
case {0} % no iteration
icnt = 0;
% find bhat's
si = 1;
sj = 1;
for i=1:neqs;
 bhat = zeros(nxi(i,1),1);
 for j=1:neqs;
  xpy = xy(sj:sj+nxj(1,j)-1,1);
  xpxi = xxi(si:si+nxi(i,1)-1,sj:sj+nxj(1,j)-1);
  bhat = bhat + xpxi*xpy;
    sj = sj+nxj(1,j);
   end;
   % save bhat's
   result(i).beta = bhat;
 sj = 1;
 si = si+nxi(i,1);
end;

% compute sur residuals
emat = zeros(nobs,neqs);
for i=1:neqs;
 result(i).resid = y(i).eq - x(i).eq*result(i).beta;
 emat(:,i) = result(i).resid;
end;

% compute sur sigma
sigma = zeros(neqs,neqs);
for i=1:neqs;
 for j=i:neqs;
    sigma(i,j) = (emat(:,i)'*emat(:,j))/nobs;
    if j > 1;
  sigma(j,i) = sigma(i,j);
    end;
end;
end;

case {1} % iteration
convg = 1000;
icnt = 0;
while convg > crit

% find bhat's
si = 1;
sj = 1;
for i=1:neqs;
 bhat = zeros(nxi(i,1),1);
 for j=1:neqs;
  xpy = xy(sj:sj+nxj(1,j)-1,1);
  xpxi = xxi(si:si+nxi(i,1)-1,sj:sj+nxj(1,j)-1);
  bhat = bhat + xpxi*xpy;
    sj = sj+nxj(1,j);
   end;
   % save bhat's
   if icnt > 0
   bsave(i).beta = result(i).beta;
   end;
   result(i).beta = bhat;
 sj = 1;
 si = si+nxi(i,1);
end;


% compute sur residuals
emat = zeros(nobs,neqs);
for i=1:neqs;
 result(i).resid = y(i).eq - x(i).eq*result(i).beta;
 emat(:,i) = result(i).resid;
end;

% compute sur sigma
sigma = zeros(neqs,neqs);
for i=1:neqs;
 for j=i:neqs;
    sigma(i,j) = (emat(:,i)'*emat(:,j))/nobs;
    if j > 1;
  sigma(j,i) = sigma(i,j);
    end;
end;
end;

% check for convergence
if icnt > 0
 convg = 0.0;
 in8.fmt = '%16.8f';
  for i=1:neqs
  convg = convg + sum(abs(result(i).beta - bsave(i).beta));
  end;
end;


icnt = icnt + 1;
if icnt > itmax
warn('sur: more than %d iterations',itmax);
break;
end;


sigmai = inv(sigma); % find sigma-inverse
% fill-in sig*(x'x) matrix
nx = 0;              % total number of x-variables in all equations
nxi = zeros(neqs,1); % left dimension of xx

for i=1:neqs;
 nx = nx + result(i).nvar;
 nxi(i,1) = result(i).nvar;
end;

nxj = nxi'; % right dimension of xx
xx = zeros(nx,nx);
si = 1;
sj = 1;
for i=1:neqs;
 for j=1:neqs;
  xx(si:si+nxi(i,1)-1,sj:sj+nxj(1,j)-1) = sigmai(i,j)*(x(i).eq'*x(j).eq);
  sj = sj+nxj(1,j);
 end;
 si = si+nxi(i,1);
 sj = 1;
end;

xxi = inv(xx); % find xpx-inverse for inference

% fill-in xpy vector
si = 1;
sj = 1;
xy = zeros(nx,1);

for i=1:neqs;
 xp = x(i).eq';
 for j=1:neqs;
   yt = y(j).eq;
  xy(si:si+nxi(i,1)-1,1) = xy(si:si+nxi(i,1)-1,1) + sigmai(i,j)*xp*yt;
   end;
 si = si+nxi(i,1);
end;


end; % end of while statement for iteration


otherwise

end; % end of switch

result(1).ccor = corrcoef(emat); % return cross-equation correlations
result(1).sigma = sigma;         % return croos-equation covariances
sigmai = inv(sigma);

% compute sur var-cov matrix
xx = zeros(nx,nx);
si = 1;
sj = 1;
for i=1:neqs;
 for j=1:neqs;
  xx(si:si+nxi(i,1)-1,sj:sj+nxj(1,j)-1) = sigmai(i,j)*(x(i).eq'*x(j).eq);
  sj = sj+nxj(1,j);
 end;
 si = si+nxi(i,1);
 sj = 1;
end;

xxi = inv(xx); % find sur xpx-inverse for inference

result(1).xxi = xxi;

% compute t-statistics
vcov = diag(xxi);

si = 1;
for i=1:neqs;
 stdb = sqrt(vcov(si:si+nxi(i,1)-1,1));
 result(i).tstat = result(i).beta./stdb;
 result(i).resid = y(i).eq - x(i).eq*result(i).beta;
 si = si+nxi(i,1);
end;

% compute t-probabilities
for i=1:neqs;
 result(i).tprob = tdis_prb(result(i).tstat,nobs-nxi(i,1));
end;

% compute overall r-squared statistic
% see Green, 1997 page 679
ymean = zeros(neqs,1);
iota = ones(nobs,1);
for i=1:neqs;
 ymean(i,1) = mean(result(i).y);
end;

for i=1:neqs;
 for j=1:neqs;
  temp = (1/nobs)*(y(i).eq-ymean(i,1)*iota).*(y(j).eq-ymean(j,1)*iota);
  syy(i,j) = sum(temp);
  if j > i
   syy(j,i) = syy(i,j);
  end;
 end;
end;

sys_rsqr = 1 - neqs/trace(sigmai*syy);

% fill-in results for each equation
   for i=1:neqs;
   yd =  result(i).y - ones(nobs,1)*ymean(i,1);
   rsqr2 = yd'*yd;
   sigu = result(i).resid'*result(i).resid;
   result(i).rsqr = 1 - sigu/rsqr2;           % r-squared
   rsqr1 = sigu/(nobs-result(i).nvar);
   rsqr2 = rsqr2/(nobs-1.0);
   result(i).rbar = 1 - (rsqr1/rsqr2);        % r-bar squared
   result(i).srsqr = sys_rsqr;
 result(i).yhat = x(i).eq*result(i).beta;
 result(i).resid = result(i).y - result(i).yhat;
 sigu = result(i).resid'*result(i).resid;
 result(i).sige = sigu/nobs;                % sige's
   ediff = result(i).resid(2:nobs) - result(i).resid(1:nobs-1);
   result(i).dw = (ediff'*ediff)/sigu;           % durbin-watson 
end;

result(1).iter = icnt;
if itflag ~= 0
result(1).convg = convg;
end;
