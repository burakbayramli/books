function s=bessel_0(x)
% s=bessel_0(x)
% Zero-order Bessel function of the first kind.
% For each element of x, x(i),
% s(i)=1+sum from k=1 to inf {[((x(i)/2)^k)/k!]^2}.
% x can be any real vector, but not a matrix.
% The sum stops when the terms become negligible.
% This algorithm is due originally to J. F. Kaiser.
%
% See also: window
N=length(x);
s=ones(1,N);
for i=1:N,
   term=1;
   n=0;
   while (term>1.e-8*s),
      n=n+2;
      term=term*((x(i)/n).^2);
      s(i)=s(i)+term;
   end
end