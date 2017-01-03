function h = firkais(typ,par,theta,deltap,deltas);
% Synopsis: h = firkais(typ,par,theta,deltap,deltas).
% Designs a FIR filter of one of the six basic types by
% Kaiser window, to meet prescribed specifications.
% Input parameters:
% typ: the filter type:
%      'l','h','p','s' for LP, HP, BP, BS, respectively,
%      'b' for Hilbert transformer, 'd' for differentiator
% par: 'e' for even order (type I or III), 
%      'o' for odd order (type II or IV)
% theta: vector of band-edge frequencies in increasing order.
% deltap: one or two pass-band tolerances
% deltas: one or two stop-band tolerances; not needed for
%         typ = 'b' or typ = 'd'
% Output:
% h: the filter coefficients.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

if (nargin == 4), deltas = deltap; end
if (typ == 'p' | typ == 's'),
   if (length(deltap) == 1), deltap = deltap*[1,1]; end
   if (length(deltas) == 1), deltas = deltas*[1,1]; end
end
[N,alpha] = kaispar(typ,par,theta,deltap,deltas);
while (1),
   if (alpha == 0), win = window(N+1,'rect');
   else, win = window(N+1,'kais',alpha); end
   if (typ=='l'), h = firdes(N,[0,mean(theta),1],win);
   elseif (typ=='h'), h = firdes(N,[mean(theta),pi,1],win);
   elseif (typ=='p'),
   h = firdes(N,[mean(theta(1:2)),mean(theta(3:4)),1],win);
   elseif (typ=='s'),
   h = firdes(N, ...
       [0,mean(theta(1:2)),1; mean(theta(3:4)),pi,1],win);
   elseif (typ=='b' | typ=='d'),
   h = diffhilb(typ,N,win); end
   res = verspec(h,typ,theta,deltap,deltas);
   if (res==0), break;
   elseif (res==1), N = N+2;
   else, alpha = alpha+0.05; end
end

