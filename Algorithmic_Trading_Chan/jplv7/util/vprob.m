function val = vprob(ev,he)
% PURPOSE: returns val = (1/sqrt(2*pi*he))*exp(-0.5*ev*ev/he)
% -----------------------------------------------------------
% USAGE: val = vprob(ev,he)
% where ev = scalar (mean)
%       he = scale  (variance)
% -----------------------------------------------------------
% RETURNS: val = (1/sqrt(2*pi*he))*exp(-0.5*ev*ev/he)

val = (1/sqrt(2*pi*he))*exp(-0.5*ev*ev/he);
