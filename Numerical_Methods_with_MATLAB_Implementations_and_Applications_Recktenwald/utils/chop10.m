function c = chop10(x,n)
% chop10   Round a floating point number to n base-10 digits.
%
% Synopsis:  c = chop10(x)
%            c = chop10(x,n)
%
% Input: x = value to be rounded
%        n = (optional) number of decimal digits retained.  Default: n = 4
%
% Output: c = value of x rounded to n decimal digits

if nargin<2,  n = 4;  end
s = sign(x);
xa = abs(x);
e = fix(log10(xa));     %  Highest power of ten that is less than x
i = find(e<=0);
if ~isempty(i)
  e(i) = e(i) - 1;      %  Shift by extra factor of 10 for abs(x)<1
end                     %  So that rounding occurs on correct digit
m = xa./(10.^e) ;                  %  Mantissa of x   
mr = round(m*10^(n-1))/10^(n-1);   %  Rounded mantissa
c = sign(x).*mr.*10.^e;
