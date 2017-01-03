function c1 = ham_itrans(c0);
% PURPOSE: inverse transform Hamilton model parameters
% ---------------------------------------------
% Usage: out = ham_itrans(parm);
% where: parm = a vector of input probabilities
% ---------------------------------------------
% RETURNS: out = a vector of inverse transformed parameters
% out = -log((1-parm)/parm)
% ---------------------------------------------
% NOTE: only first 2 elements of parm are transformed
%       remaining elements are untouched
% ---------------------------------------------
% SEE ALSO: ham_trans, this function reverses ham_trans()
% ---------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

c1=c0;

for i=1:2;
c1(i,1) = -log((1-c0(i,1))/c0(i,1));
end;

