function I = humpInt(a,b)
% humpInt  Exact value of integral of humps function on [a,b]
%
% Synopsis:  I = humpInt(a,b)
%
% Input:  a,b = limits of integral
%
% Output:  I = integral from a to b of humps(x)*dx

c = [0.3  0.01  0.9  0.04  -6];    %  constants in humps function
I =   ( atan((c(1)-a)/sqrt(c(2))) - atan((c(1)-b)/sqrt(c(2))) )/sqrt(c(2)) ...
    + ( atan((c(3)-a)/sqrt(c(4))) - atan((c(3)-b)/sqrt(c(4))) )/sqrt(c(4)) ...
    + c(5)*(b-a);
