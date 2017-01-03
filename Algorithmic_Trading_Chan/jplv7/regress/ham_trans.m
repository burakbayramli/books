function c1 = ham_trans(c0);
% PURPOSE: transform Hamilton model parameters
% ---------------------------------------------
% Usage: out = ham_trans(parm);
% where: parm = a vector of input parameters
% ---------------------------------------------
% RETURNS: out = a vector of probabilities
% % out = exp(parm/(1+exp(parm));
% ---------------------------------------------
% NOTE: only first 2 elements of parm are transformed
%       remaining elements are untouched
% ---------------------------------------------
% SEE ALSO: ham_itrans, the reverse transformation
% ---------------------------------------------


c1=c0;

for i=1:2;
c1(i,1) = exp(c0(i,1))/(1+exp(c0(i,1)));
end;

