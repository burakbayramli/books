function [c,n] = clsq(A,dim);
% solves the constrained least squares Problem
% A (c n)' ~ 0 subject to norm(n,2)=1
% length(n) = dim
% [c,n] = clsq(A,dim)
[m,p] = size(A);
if p < dim+1, error ('not enough unknowns'); end;
if m < dim, error ('not enough equations'); end;
m = min (m, p);
R = triu (qr (A));
[U,S,V] = svd(R(p-dim+1:m,p-dim+1:p));
n = V(:,dim);
c = -R(1:p-dim,1:p-dim)\R(1:p-dim,p-dim+1:p)*n;
