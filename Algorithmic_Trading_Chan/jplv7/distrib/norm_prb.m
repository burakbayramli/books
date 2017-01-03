function p=norm_prb(tratio)
% PURPOSE: computes normal probability given
%          x/std(x) in lieu of t-statistic probabilities
%          for cases involving asymptotic t-ratios
% ---------------------------------------------------
% USAGE: prob = norm_prb(tratio)
% where: tratio = bhat/std(bhat)
% ---------------------------------------------------
% RETURNS: prob = marginal probability 
% ---------------------------------------------------
% NOTE: test the null hypothesis beta = 0 (2-tailed)
% ---------------------------------------------------
% SEE ALSO: tdis_prb, fdis_prb, chis_prb  
% ---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

%Defaults
if nargin ~= 1
error('Wrong # of arguments to norm_prb');
end;

mu=0; 

tratio = abs(tratio);

p = 2*(1-0.5*(1+erf( tratio*(1/sqrt(2)))));

big=find(p>1);
if any(big), 
   p(big)=ones(size(big)); 
end;
