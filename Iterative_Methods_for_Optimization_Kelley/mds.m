function [x,lhist,histout]=mds(x0,f,tol,maxit,budget)
%
% Multidirectional search
%
% C. T. Kelley, July 17, 1998
%
%
% This code comes with no guarantee or warranty of any kind.
%
% function [x,lhist,histout] = mds(x0,f,tol,maxit,budget)
%
% inputs:
%	vertices of initial simplex = x0 (n x n+1 matrix)
%          The code will order the vertices for you and no benefit is
%          accrued if you do it yourself.
%
%       objective function = f
%
%       termination tolerance = tol
%       maximum number of iterations = maxit (default = 100)
%           As of today, dist = | best value - worst value | < tol
%           or when maxit iterations have been taken
%       budget = max f evals (default=50*number of variables)
%                 The iteration will terminate after the iteration that
%                 exhausts the budget
%
%
% outputs:
%	final simplex = x (n x n+1) matrix
%       lhist = number of iterations before termination
%       iteration histor = histout itout x 4
%         histout = iteration history, updated after each nonlinear iteration
%                 = lhist x 4 array, the rows are
%                   [fcount, fval, norm(grad), dist, diam]
%                   fcount = cumulative function evals
%                   fval = current best function value
%                   dist = difference between worst and best values
%                   diam = max oriented length
%
% initialize counters
%
lhist=0; fcount=0;
%
% set debug=1 to print out iteration stats
%
debug=0;
%
% Set the MDS parameters
%
de=2; dc=.5; 
%
% cache control paramters
%
global cache_size cache cache_ptr cache_fvals
[n,m]=size(x0); 
cache_size=4*n; cache=zeros(n,cache_size); cache_ptr=0;
cache_fvals=zeros(cache_size,1);
%
if nargin < 4 maxit=100; end
if nargin < 5 budget=100*n; end
%
% Order the vertices for the first time
%
x=x0; 
histout=[];
itout=0; orth=0; fv=zeros(n+1,1);
xtmp=zeros(n,n+1); z=zeros(n,n); delf=zeros(n,1);
for j=1:n+1; fv(j)=geval(f,x(:,j)); 
end; fcount=fcount+n+1;
[fs,is]=sort(fv); xtmp=x(:,is); x=xtmp; fv=fs;
itc=0; dist=fv(n+1)-fv(1);
diam=zeros(n,1);
for j=2:n+1
   v(:,j-1)=-x(:,1)+x(:,j);
   diam(j-1)=norm(v(:,j-1));
end
lhist=lhist+1;
thist=[fcount, fv(1), dist, max(diam)]; histout=[histout',thist']';
%
% main MDS loop
%
while(itc < maxit & dist > tol & fcount <= budget)
happy=0;
%
%  reflect
%
for j=2:n+1 xr(:,j)=x(:,1) - v(:,j-1); 
[fr(j),ctr] = geval(f,xr(:,j)); 
fcount=fcount+ctr;
end
%fcount=fcount+n; 
fvr=min(fr(2:n+1)); 
if fv(1) > fvr
    happy=1;
%
% expand
%
    for j=2:n+1 xe(:,j)=x(:,1) - de*v(:,j-1); fe(j) = feval(f,xe(:,j)); end
    fcount=fcount+n; fve=min(fe(2:n+1));
    if fvr > fve
        for j=2:n+1 x(:,j)=xe(:,j); fv(j)=fe(j); end
    else
        for j=2:n+1 x(:,j)=xr(:,j); fv(j)=fr(j); end
    end
end
%
% contract
%
if happy==0
   for j=2:n+1 x(:,j)=x(:,1) + dc*v(:,j-1); 
   [fv(j),ctr] = geval(f,x(:,j)); fcount=fcount+ctr;
   end
%   fcount=fcount+n;
end
%
%  sort the new vertices
%
   [fs,is]=sort(fv); xtmp=x(:,is); x=xtmp; fv=fs;
%
%  compute the diameter of the new simplex and the iteration data
%
   for j=2:n+1
       v(:,j-1)=-x(:,1)+x(:,j);
       diam(j-1)=norm(v(:,j-1));
   end
   dist=fv(n+1)-fv(1);
   lhist=lhist+1;
   thist=[fcount, fv(1), dist, max(diam)]; histout=[histout',thist']';
end
%
%
%
function [fs,ctr]=geval(fh,xh)
global cache_size cache cache_ptr cache_fvals
for i=1:cache_size
    nz(i)=norm(xh-cache(:,i));
end
[vz,iz]=min(nz);
if vz == 0 & cache_ptr ~=0
    fs=cache_fvals(iz);
    ctr=0;
else
    fs=feval(fh,xh);
    ctr=1;
    cache_ptr=mod(cache_ptr,cache_size)+1;
    cache(:,cache_ptr)=xh;
    cache_fvals(cache_ptr)=fs;
end
