function [s,t,w]=gausspoints_twod(oneg,onew) 
%GAUSSPOINTS_TWOD constructs tensor product Gauss Point Rule
% [s,t,w] = gausspoints_twod(oneg,onew);
%  input
%   oneg    1D Gaussian Points in (-1,1)
%   onew    Weights in 1D
%  output
%   s       x-coordinate of the Gaussian Points
%   t       y-coordinate of the Gaussian Points
%   w       weights in 2D
%    IFISS function: DJS; 2 January 2011.
%  Copyright (c) 2010 D.J. Silvester, Qifeng Liao 
ng=max(size(oneg)); 
s=zeros(ng*ng,1);
t=zeros(ng*ng,1);
w=zeros(ng*ng,1);
for k=1:ng
    for j=1:ng
        s(j+(k-1)*ng)=oneg(k);
        t(j+(k-1)*ng)=oneg(j);
        w(j+(k-1)*ng)=onew(k)*onew(j);
    end
end
return
