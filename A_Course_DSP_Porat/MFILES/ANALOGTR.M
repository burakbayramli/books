function [b,a,vout,uout,Cout] = analogtr(typ,vin,uin,Cin,w);
% Synopsis: [b,a,vout,uout,Cout] = analogtr(typ,vin,uin,Cin,w).
% Performs frequency transformations of analog low-pass filters.
% Input parameters:
% typ: the transformation type:
%      'l' for low-pass to low-pass
%      'h' for low-pass to high-pass
%      'p' for low-pass to band-pass
%      's' for low-pass to band-stop
% vi, ui, Cin: the poles, zeros, and constant gain of the low-pass
% w: equal to omega_c for 'l' or 'h'; a 1 by 2 matrix of
%    [omega_l, omega_h] for 'p' or 's'.
% Output parameters:
% b, a: the output polynomials
% vout, uout, Cout: the output poles, zeros, and constant gain.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

p = length(vin); q = length(uin);
if (typ == 'l'),
   uout = w*uin; vout = w*vin; Cout = w^(p-q)*Cin;
elseif (typ == 'h'),
   uout = [w./uin,zeros(1,p-q)]; vout = w./vin;
   Cout = prod(-uin)*Cin/prod(-vin);
elseif (typ == 'p'),
   wl = w(1); wh = w(2); uout = []; vout = [];
   for k = 1:q,
      uout = [uout,roots([1,-uin(k)*(wh-wl),wl*wh]).']; end
   uout = [uout,zeros(1,p-q)];
   for k = 1:p,
      vout = [vout,roots([1,-vin(k)*(wh-wl),wl*wh]).']; end
   Cout = (wh-wl)^(p-q)*Cin;
elseif (typ == 's'),
   [t1,t2,t3,t4,t5] = analogtr('h',vin,uin,Cin,1);
   [t1,t2,vout,uout,Cout] = analogtr('p',t3,t4,t5,w);
end
a = 1; b = 1;
for k = 1:length(vout), a = conv(a,[1,-vout(k)]); end
for k = 1:length(uout), b= conv(b,[1,-uout(k)]); end
a = real(a); b = real(Cout*b); Cout = real(Cout);

