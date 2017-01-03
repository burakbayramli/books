function w = wish_rnd(sigma,v)
% PURPOSE: generate random wishart matrix
%------------------------------------------------
% USAGE:     w = wish_rnd(sigma,n)
% Where: sigma = symmetric pds input matrix
%            n = degrees of freedom parameter
%------------------------------------------------
% RETURNS:
%        w = random wishart_n(sigma)
%             distributed matrix
%------------------------------------------------
% REFERENCES: Gelman, Carlin, Stern, Rubin, Bayesian Data 
%             Analysis, (1995,96) pages 474, 480-481.

% written by:
% Aki Vehtari
% Helsinki University of Technology 
% Lab. of Computational Engineering 
% P.O.Box 9400 
% FIN-02015 HUT 
% FINLAND
% Aki.Vehtari@hut.fi

if nargin ~= 2
  error('Wrong # of arguments to wish_rnd');
end;

[n k] = size(sigma);

if n ~= k
  error('wish_rnd: requires a square matrix');
elseif n < k
  warning('wish_rnd: n must be >= k+1 for a finite distribution');
end;

[t,p]=chol(sigma);
if p < 0
  error('wish_rnd: matrix must be a positive definite');
end

y = t'*randn(n,v);
w = y*y';
