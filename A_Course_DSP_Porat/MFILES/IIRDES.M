function [b,a,v,u,C] = iirdes(typ,band,theta,deltap,deltas);
% Synopsis: [b,a,v,u,C] = iirdes(typ,band,theta,deltap,deltas).
% Designs a digital IIR filter to meet given specifications.
% Input parameters:
% typ: the filter type: 'but', 'ch1', 'ch2', or 'ell'
% band: 'l' for LP, 'h' for HP, 'p' for BP, 's' for BS
% theta: an array of band-edge frequencies, in increasing
%        order; must have 2 frequencies if 'l' or 'h',
%        4 if 'p' or 's'
% deltap: pass-band ripple/s (possibly 2 for 's')
% deltas: stop-band ripple/s (possibly 2 for 'p')
% Output parameters:
% b, a: the output polynomials
% v, u, C: the output poles, zeros, and constant gain.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

% Prewarp frequencies (with T = 1)
omega = 2*tan(0.5*theta);
% Transform specifications
if (band == 'l'), wp = omega(1); ws = omega(2);
elseif (band == 'h'), wp = 1/omega(2); ws = 1/omega(1);
elseif (band == 'p'),
   wl = omega(2); wh = omega(3); wp = 1;
   ws = min(abs((omega([1,4]).^2-wl*wh) ...
        ./((wh-wl)*omega([1,4]))));
elseif (band == 's'),
   wl = omega(2); wh = omega(3); ws = 1;
   wp = 1/min(abs((omega([1,4]).^2-wl*wh) ...
        ./((wh-wl)*omega([1,4]))));
end
% Get low-pass filter parameters
[N,w0,epsilon,m] = lpspec(typ,wp,ws,min(deltap),min(deltas));
% Design low-pass filter
[b,a,v1,u1,C1] = analoglp(typ,N,w0,epsilon,m);
% Transform to the required band
ww = 1; if (band == 'p' | band == 's'), ww = [wl,wh]; end
[b,a,v2,u2,C2] = analogtr(band,v1,u1,C1,ww);
% Perform bilinear transformation
[b,a,v,u,C] = bilin(v2,u2,C2,1);

