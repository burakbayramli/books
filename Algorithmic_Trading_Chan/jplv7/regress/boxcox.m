function results = boxcox1(y,x,lamlo,lamup,model,foptions)
% PURPOSE: box-cox regression using a single scalar transformation
%          parameter for both y and (optionally) x
% -----------------------------------------
% USAGE: results = boxcox1(y,x,lam_lo,lam_up,model,foptions)
%      where: y = dependent variable vector
%             x = explanatory variables matrix
%                 (intercept vector in 1st column --- if desired)
%        lam_lo = scalar, lower limit for simplex search
%        lam_up = scalar, upper limit for simplex search  
%        model  = 0 for y-transform only
%               = 1 for both y, and x-transform
%     foptions  = (optional) structure OPTIONS, created with
%                    the OPTIMSET function.  See OPTIMSET for details.
% -----------------------------------------
% RETURNS: a structure:
%        results.meth  = 'boxcox'
%        results.beta  = bhat estimates
%        results.lam   = lamda estimate
%        results.tstat = t-stats for bhat
%        results.yhat  = yhat (box-cox transformed)
%        results.resid = residuals
%        results.sige  = e'*e/(n-k)
%        results.rsqr  = rsquared
%        results.rbar  = rbar-squared
%        results.nobs  = nobs
%        results.nvar  = nvars
%        results.y     = y data vector (box-cox transformed)
%        results.iter  = # of iterations
%        results.like  = -log likelihood function value
% --------------------------------------------------
% NOTE: uses MATLAB simplex optimization function fmin
% --------------------------------------------------
% SEE ALSO: prt(results), plt(results), boxcox2()
% ---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


[n k] = size(x);
results.nobs = n;
results.nvar = k;
results.meth = 'boxcox';
ncheck = find(y > 0);
if ncheck ~= n
   error('box_cox: all y-values must be positive');
end;

if nargin == 6
 if model == 0
  % call simplex optimization with user-supplied options
  % and untransformed x-matrix
  [lam fval exitf out] = fminbnd('box_lik',lamlo,lamup,foptions,y,x,model);
  niter = out.iterations;
  llike = -fval;
  
  elseif model == 1
  % error check on positive x-values
  ncheck = find(x > 0);
   if ncheck ~= n*k
   error('box_cox: all x-values must be positive');
   end;

  [lam fval exitf out] = fminbnd('box_lik',lamlo,lamup,foptions,y,x,model); 
niter = out.iterations;
llike = -fval;
  
 else
 error('box_cox: only model=0,1 values allowed');
 end;

elseif nargin == 5
 % call simplex optimization with default options
% turn off display
foptions = optimset('Display','off');
 [lam fval exitf out] = fminbnd('box_lik',lamlo,lamup,foptions,y,x,model);
niter = out.iterations;
llike = -fval;

else
 error('box_cox: Wrong # of input arguments');
end;

if exitf == 0
error('box_cox: no convergence in %d iterations \n',niter);
exit;
end;

ys = boxc_trans(y,lam);
if model == 1
   % see if an intercept term exists in the model
   iota = x(:,1);
   ifind = find(iota == 1);
   if isempty(ifind) % no intercept
     xs = boxc_trans(x,lam);
   else % we may have an intercept       
    if length(ifind) == n % we have an intecept
     xtrans = boxc_trans(x(:,2:k),lam);
     xs = [ones(n,1) xtrans];
    else % no intercept
     xs = boxc_trans(x,lam);
    end; 
   end; 
elseif model == 0
    xs = x;
end;

xpxi = inv(xs'*xs);
bhat = xpxi*xs'*ys;
e = ys - xs*bhat;
sigu = e'*e;
sige = sigu/n;

% compute t-statistics
bvar = sige*xpxi;
tstat = bhat./sqrt(diag(bvar));
results.tstat = tstat;
results.yhat = x*bhat;
results.resid = ys - results.yhat;
ym = ys - ones(n,1)*mean(ys);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(n-k);
rsqr2 = rsqr2/(n-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared
results.y = ys;
results.beta = bhat;
results.lam = lam;
results.sige = sige;
results.iter = niter;
results.like = llike;

% The following function was cut and save in a new function file 
% named boxc_trans

% function bdata = boxc_trans(x,lam)
% % PURPOSE: compute box-cox transformation
% %----------------------------------------------------
% % USAGE: bdata = boxc_trans(data,lam)
% % where:    lam  = scalar transformation parameter
% %           data = matrix nobs x k
% %----------------------------------------------------
% % RETURNS: bdata = data matrix box-cox transformed
% 
% % written by:
% % James P. LeSage, Dept of Economics
% % University of Toledo
% % 2801 W. Bancroft St,
% % Toledo, OH 43606
% % jlesage@spatial-econometrics.com
% 
% [n k] = size(x);
% z = zeros(n,k);
% iota = ones(n,1);
% 
%  for i=1:k;
%    if lam ~= 0
%    z(:,i) = (x(:,i).^lam - iota)/lam;
%    else
%    z(:,i) = log(abs(x(:,i)));
%    end;
%  end;
% 
% bdata = z;

