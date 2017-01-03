function q = scoreq(qmin,qmax,y,x,east,north)
% PURPOSE: evaluates cross-validation score for optimal q in gwr
%          based on tricube weighting
% ------------------------------------------------------
% USAGE: score = scoreq(qmin,qmax,y,x,east,north);
% where: qmin = minimum # nearest neighbors to use in CV search
%        qmax = maximum # nearest neighbors to use in CV search     
%        y    = dependent variable
%        x = matrix of explanatory variables
%     east = longitude (x-direction) coordinates
%    north = lattitude (y-direction) coordinates
% ------------------------------------------------------
% RETURNS: q = # of nearest neighbors that minimum the score
%              function
% ------------------------------------------------------
% NOTE: this function catches inversion problems
%       and uses Hoerl-Kennard ridge regression
%       if needed
% ------------------------------------------------------
% SEE ALSO: scoref which finds optimal bandwidth
%           for gaussian and exponential weighting
% ------------------------------------------------------
          
% written by: James P. LeSage 2/98
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[n k] = size(x); res = zeros(n,1);
d = zeros(n,1);
qgrid = qmin:qmax;
nq = length(qgrid);
wt = zeros(n,nq); 
for iter = 1:n;
 dx = east - east(iter,1);
 dy = north - north(iter,1);
 d = (dx.*dx + dy.*dy);
         % sort distance to find q nearest neighbors
         ds = sort(d); 
         dmax = ds(qmin:qmax,1); 
         for j=1:nq;
   nzip = find(d <= dmax(j,1));
          wt(nzip,j) = (1-(d(nzip,1)/dmax(j,1)).^3).^3;
   wt(iter,j) = 0.0;
         end; % end of j loop
for j=1:nq;
% computational trick to speed things up 
% use wt non-zero to pull out y,x observations
nzip = find(wt(:,j) > 0);
ys = y(nzip,1).*sqrt(wt(nzip,j));
xs = matmul(x(nzip,:),sqrt(wt(nzip,j)));
bi=xs\ys;
% compute predicted values
yhat = x(iter,:)*bi;
% compute residuals 
res(iter,j) = y(iter,1) - yhat;
end; % end of for j loop over q-values
end; % end of for iter loop


tmp = res.*res;
score = sum(tmp);
[smin sind] = min(score);
q = qgrid(sind);

