function f = bin2flt(c)
% bin2flt  Expand a binary representation of floating point mantissa
%
% Synopsis:  f = bin2flt(c)
%
% Input:     c = (string) bit sequence of the fractional part of a normalized
%                floating point number.
%
% Output     f = numerical equivalent of the bit pattern
%                      f = sum(k=1:n){ c(k)*2^(-k) }

f=0;
for k=1:length(c)
  f = f + str2num(c(k))*2^(-k);
end
