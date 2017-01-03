function [flag,y] = lc2sim(qtype,when,rtype,apar,B,s0,n);
% Synopsis: [flag,y] = lc2sim(qtype,when,rtype,apar,B,s0,n).
% Zero-input limit cycle simulation for a 2nd-order filter.
% Input parameters:
% qtype: 't': truncate, 'r': round, 'm': magnitude truncate
% when:  'b': quantize before summation, 'a': after
% rtype: 'd': direct realization, 'c': coupled realization
% apar:  [a1, a2] for direct, [alphar, alphai] for coupled
% B:     number of bits, s0:    initial state
% n:     maximum number of time points to simulate.
% Output parameters:
% flag:  0: no LC, 1: DC LC, 2: other LC
% y:     the output signal.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

s = [quant(s0(1),qtype,B), quant(s0(2),qtype,B)]; sp = s;
apar(2) = quant(apar(2),'r',B);
if (abs(apar(1)) >= 1), apar(1) = 2*quant(apar(1)/2,'r',B);
else, apar(1) = quant(apar(1),'r',B); end
y = zeros(1,n); flag = 2;
for i = 1:n,
if (rtype == 'd'),
temp1 = -apar(1)*s(1); temp2 = -apar(2)*s(2); s(2) = s(1);
if (when == 'b'),
   s(1) = quant(temp1,qtype,B) + quant(temp2,qtype,B);
else, s(1) = quant(temp1+temp2,qtype,B); end; y(i) = s(1);
else,
temp1 = apar(1)*s(1); temp2 = apar(2)*s(2);
temp3 = -apar(2)*s(1); temp4 = apar(1)*s(2);
if (when == 'b'),
   s(1) = quant(temp1,qtype,B) + quant(temp2,qtype,B);
   s(2) = quant(temp3,qtype,B) + quant(temp4,qtype,B);
else,
   s(1) = quant(temp1+temp2,qtype,B);
   s(2) = quant(temp3+temp4,qtype,B);
end; y(i) = s(1);
end
if (s(1) == 0 & s(2) == 0),
   flag = 0; y = y(1:i); break; end
if (s(1) == sp(1) & s(2) == sp(2)),
   flag = 1; y = y(1:i); break; end
sp = s; end

