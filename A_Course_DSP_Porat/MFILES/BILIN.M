function [b,a,vout,uout,Cout] = bilin(vin,uin,Cin,T);
% Synopsis: [b,a,vout,uout,Cout] = bilin(vin,uin,Cin,T).
% Computes the bilinear transform of an analog filter.
% Input parameters:
% vi, ui, Cin: the poles, zeros, and constant gain of the
%              analog filter
% T: the sampling interval.
% Output parameters:
% b, a: the output polynomials
% vout, uout, Cout: the output poles, zeros, and constant gain.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

p = length(vin); q = length(uin);
Cout = Cin*(0.5*T)^(p-q)*prod(1-0.5*T*uin)/prod(1-0.5*T*vin);
uout = [(1+0.5*T*uin)./(1-0.5*T*uin),-ones(1,p-q)];
vout = (1+0.5*T*vin)./(1-0.5*T*vin);
a = 1; b = 1;
for k = 1:length(vout), a = conv(a,[1,-vout(k)]); end
for k = 1:length(uout), b= conv(b,[1,-uout(k)]); end
a = real(a); b = real(Cout*b); Cout = real(Cout);

