function [N,alpha] = kaispar(typ,par,theta,deltap,deltas);
% Synopsis: kaispar(typ,par,theta,deltap,deltas).
% Estimates parameters for FIR Kaiser window filter design.
% Input parameters: see description in firkais.m.
% Output parameters:
% N: the filter order
% alpha: the Kaiser window parameter.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

A = -20*log10(min([deltap,deltas]));
if (A > 50), alpha = 0.1102*(A-8.7);
elseif (A > 21), alpha = 0.5842*(A-21)^(0.4)+0.07886*(A-21);
else, alpha = 0; end
if (typ == 'b'), dt = theta;
elseif (typ == 'd'), dt = (pi-theta);
else,
   if (length(theta) == 2), dt = theta(2)-theta(1);
   else, dt = min(theta(2)-theta(1),theta(4)-theta(3)); end
end
N = ceil((A-7.95)/(2.285*dt)); Npar = rem(N,2);
oddpermit = (par=='o') & (typ~='h') & (typ~='s');
if (Npar ~= oddpermit), N = N+1; end

