function score = scoref(bdwt,y,x,east,north,flag)
% PURPOSE: evaluates cross-validation score for optimal gwr bandwidth
%          with gauusian or exponential weighting
% ------------------------------------------------------
% USAGE: score = scoref(y,x,east,north,bdwt);
% where: y = dependent variable
%        x = matrix of explanatory variables
%     east = longitude (x-direction) coordinates
%    north = lattitude (y-direction) coordinates
%     bdwt = a bandwidth to use in computing the score
%     flag = 0 for Gaussian weights
%          = 1 for BFG exponential
% ------------------------------------------------------
% RETURNS: score = a cross-validation criterion
% ------------------------------------------------------
% SEE ALSO: scoreq that determines optimal q-value for
%           tricube weighting
% ------------------------------------------------------

% written by: James P. LeSage 2/98
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[n k] = size(x); res = zeros(n,1);
wt = zeros(n,1); d = zeros(n,1);
for iter = 1:n;
   dx = east - east(iter,1);
   dy = north - north(iter,1);
   d = (dx.*dx + dy.*dy);
   sd = std(sqrt(d)); 
   if flag == 0,     % Gausian weights 
  wt = stdn_pdf(sqrt(d)/(sd*bdwt));
   elseif flag == 1, % exponential weights
         wt = exp(-d/bdwt);
   end;
   wt(iter,1) = 0.0;
   wt = sqrt(wt);
   
   
% computational trick to speed things up  
% use non-zero wt to pull out y,x observations
nzip = find(wt >= 0.0);
ys = y(nzip,1).*wt(nzip,1);
xs = matmul(x(nzip,:),wt(nzip,1));
[nn,kk] = size(xs);
xpxi = invpd(xs'*xs + eye(kk)*(1000*eps)); % prevent singular xpx matrix
bi = xpxi*xs'*ys;
% compute predicted values
yhat = x(iter,:)*bi;
% compute residuals 
res(iter,1) = y(iter,1) - yhat;
end; % end of for iter loop

tmp = res'*res;
score = sqrt(tmp/n);
