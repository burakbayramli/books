function xmat = ptrend(p,nobs)
% PURPOSE: produce an explanatory variables matrix
%          containing a polynomial time-trend
% ----------------------------------------------------
% USAGE: xmat = ptrend(p,nobs);
% where: p = order of the time-trend polynomial
%            p < 0, xmat = iota (nobs x 1) vector of ones
%            p = 1, xmat = time trend
%            p > 1, xmat = higher order polynomial in time
%     nobs = size of the matrix
% ----------------------------------------------------
% RETURNS: xmat = matrix containing polynomial trend model data set
% ----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatia-econometrics.com


     u = ones(nobs,1) ;
     if p > 0
      timep = zeros(nobs,p) ;
      t = 1:nobs;
      t = (t')/nobs;
       m        = 1 ;
       while (m <= p)
          timep(:,m) = t.^m ;
          m = m + 1 ;
       end;
       xmat = [u timep];
     else,
       xmat = u ;
     end;
