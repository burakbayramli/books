function h = firls(N,typ,theta,deltap,deltas);
% Synopsis: h = firls(N,typ,theta,deltap,deltas).
% Designs a FIR filter of one of the four basic types by
% least-squares.
% Input parameters:
% typ: the filter type:
%      'l','h','p','s' for LP, HP, BP, BS, respectively,
% theta: vector of band-edge frequencies in increasing order.
% deltap: one or two pass-band tolerances
% deltas: one or two stop-band tolerances.
% Output:
% h: the filter coefficients.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

thetai = (pi/(32*N)) + (pi/(16*N))*(0:(16*N-1));
if (rem(N,2)),
   F = cos(0.5*thetai); K = (N-1)/2;
else,
   F = ones(1,16*N); K = N/2;
end
if (typ == 'p' | typ == 's'),
   if (length(deltap) == 1), deltap = deltap*[1,1]; end
   if (length(deltas) == 1), deltas = deltas*[1,1]; end
end
[V,Ad] = firlsaux(typ,theta,deltap,deltas,thetai);
carray = cos(thetai'*(0:K)).*((F.*V)'*ones(1,K+1));
darray = (V.*Ad)';
g = (carray\darray)';
if (rem(N,2)),
   h = 0.25*[g(K+1),fliplr(g(3:K+1))+fliplr(g(2:K))];
   h = [h,0.25*g(2)+0.5*g(1)];
   h = [h,fliplr(h)];
else,
   h = [0.5*fliplr(g(2:K+1)),g(1),0.5*g(2:K+1)];
end

