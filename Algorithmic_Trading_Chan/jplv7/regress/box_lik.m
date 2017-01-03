function like = box_lik(lam,y,x,model);
% PURPOSE: evaluate Box-Cox model likelihood function
%----------------------------------------------------
% USAGE: like = box_lik(b,y,x,model)
% where:  lam = box-cox parameter (scalar)
%           y = dependent variable vector (un-transformed)
%   x = explanatory variables matrix (un-transformed)
%       model = 0 for y-transform only, 1 for y,x both transformed
% NOTE: x should contain intercept vector in 1st column (if desired)
%----------------------------------------------------
% RETURNS: lik = -log likelihood function

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


[n k] = size(x);
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

bhat = inv(xs'*xs)*xs'*ys;
e = ys - xs*bhat;
sige = (e'*e)/n;

like = (lam(1) - 1)*sum(log(y)) -(n/2)*log(sige);
like = -like;
