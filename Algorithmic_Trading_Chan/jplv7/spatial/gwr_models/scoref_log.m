function score = scoref_log(bdwt,y,x,east,north)
% PURPOSE: evaluates cross-validation score for optimal gwr_logit bandwidth
%          with gauusian weighting
% ------------------------------------------------------
% USAGE: score = scoref_log(bdwt,y,x,east,north);
% where: y = dependent variable
%        x = matrix of explanatory variables
%     east = longitude (x-direction) coordinates
%    north = lattitude (y-direction) coordinates
%     bdwt = a bandwidth to use in computing the score
% ------------------------------------------------------
% RETURNS: score = a cross-validation criterion
% ------------------------------------------------------

% written by: James P. LeSage 2/98
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[n k] = size(x); res = zeros(n,1);
wt = zeros(n,1); 
for iter = 1:n;
   dx = east - east(iter,1);
   dy = north - north(iter,1);
   d = (dx.*dx + dy.*dy);
   sd = std(sqrt(d)); 
   % Gausian weights 
   wt = stdn_pdf(sqrt(d)/(sd*bdwt));
   wt(iter,1) = 0.0;
wt = sqrt(wt);  
xs = matmul(x,wt);
tmp = logit(y,xs);
bi = tmp.beta;
% compute predicted values
yhat = 1/(1+exp(-x(iter,:)*bi));
% compute residuals 
res(iter,1) = y(iter,1) - yhat;
end; % end of for iter loop

tmp = res'*res;
score = sqrt(tmp/n);

