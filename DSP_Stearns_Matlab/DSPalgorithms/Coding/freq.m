function [f,xmin,xmax]=freq(x)
% [f,xmin,xmax]=freq(x)
%
% Computes the amplitude distribution of x in terms of
% frequency vector f.
%
% Input: 
%   x =any INTEGER vector or array. (Real values are rounded.)
%
% Outputs:
%   f =row vector of symbol frequencies in x.
%      f(n)=(number of occurrences of x(xmin+n-1))/length(x),
%      for n=1,2,...,xmax-xmin+1.  Note: Sum(f)=1.
%   xmin,xmax =minimun and maximum elements in x.
x=round(x(:));
xmin=min(x); xmax=max(x);
f=zeros(1,xmax-xmin+1);
for i=1:length(x),
   k=x(i)-xmin+1;
   f(k)=f(k)+1;
end
f=f/length(x);

