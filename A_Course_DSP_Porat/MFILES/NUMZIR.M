function b = numzir(a,yinit);
% Synopsis: b = numzir(a,yinit).
% Compute the numerator polynomial for finding the zero-input
% response of the homogeneous equation a(z)y(z) = 0.
% Input parameters:
% a: the coefficient polynomial of the homogeneous equation
% yinit: the vector of y[-1], y[-2], ..., y[-p].
% Output:
% b: the numerator.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

p = length(a) - 1; a = fliplr(reshape(-a(2:p+1),1,p));
b = conv(reshape(yinit,1,p),a); b = fliplr(b(1:p));

