function [y,scl,shft]=quant(x,n)
% [y,scl,shft]=quantize(x,n)
%
% Inputs:    x =vector with real elements.
%            n =number of bits per quantized element of x.
%
% Outputs:   y =quantized x with n-bit integer elements.
%               The elements of y range from 0 to 2^n-1.
%          scl =scaling factor used to approximate x from y.
%         shft =shifting factor used to approximate x from y.
%               x may be approximated by (y-shft)*scl.
if n<1 | n>32,
    error('Number of bits (n) is out of range.');
end

y=x;                        %initially, y=x
if range(x)==0,
    return                  %if range(x)=0, y=x (no change)
end
scl=range(x)/(2^n-1);
x=x/scl;
%scale x to range 2^n-1.
shft=-min(x);
x=x+shft;                   %shift so min(x)=0.
y(:)=round(x);              %convert x to integers in y.