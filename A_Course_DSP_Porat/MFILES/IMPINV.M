function [bout,aout] = impinv(bin,ain,T);
% Synopsis: [bout,aout] = impinv(bin,ain,T).
% Computes the impulse invariant transformation of an analog filter.
% Input parameters:
% bin, ain: the numerator and denominator polynomials of the
%           analog filter
% T: the sampling interval
% Output parameters:
% bout, aout: the numerator and denominator polynomials of the
%             digital filter.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

if (length(bin) >= length(ain)),
   error('Analog filter in IMPINV is not strictly proper'); end
[r,p,k] = residue(bin,ain);
[bout,aout] = pf2tf([],T*r,exp(T*p));

