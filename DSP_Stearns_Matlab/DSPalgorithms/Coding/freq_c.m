function [f,xmin,xmax]=freq_c(x)
% [f,vx]=freq_c(x)
%
% Computes the amplitude distribution of x in terms of
% frequency vector f.
%
% Input: 
%   x =any vector or array. (Real values are not rounded.)
%
% Outputs:
%   f =row vector of symbol frequencies in x.
%      f(n)={number of occurrences of x=vx(n)} / length(x),
%      for n=1,2,...,length(vx).  Note: sum(f)=1.
%   vx=values of x where f(x) is nonzero.
%      x is considered "not quantized" and an error is indicated
%      if length(vx)>nmax (nmax is set on line 1 of the function).

nmax=4096                       %nmax is set for 12-bit conversion
f=zeros(nmax,1);
vx=zeros(nmax,1);
nbins=0;
y=sort(x);                      %y values are in ascending order

unfinished


for i=1:length(x),
   k=x(i)-xmin+1;
   f(k)=f(k)+1;
end
f=f/length(x);

