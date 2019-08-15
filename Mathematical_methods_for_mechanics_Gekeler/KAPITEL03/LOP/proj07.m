function [x,errorcode] = proj07(a,B,c,l,u,p);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% revised projection method
% problem max(a*x, B*x <= c, b^ix = c^i, i = 1:p, l <= x <= u)
% B (m,n)-matrix, a,y row vectors, c,l,u,x column vectors
% phase 1: Computation of feasible point
% ASSUMPTION:
%       (a) rang(B(1:p,:) = p
%       (b) rang (B) = n
%       (c) - inf*ones(n,1) <= l, u <= inf*ones(n,1)
% OUTPUT:
%        x feasible point of primal problem (if exists)
%        errorcode = 3: max. step number reached
%        errorcode = 4: feasible domain empty
% REMARK:
%       IB is index (column) vector C in text
%       NB is index (column) vector D in text
% ---------------------------------------------------------
% uses proj06.m
% ---------------------------------------------------------
%
[m,n]       = size(B);
tol         = 100*eps;
a1          = [-1, zeros(1,n)];
if p > 0
   e         = ones(1,p);
   a1        = [-(p+1), e*B(1:p,:)];
end
B1          = [-ones(m,1), B];
R           = find(l ~= - inf);
S           = find(u ~= inf);
x           = zeros(n,1);
x(S)        = u(S);
x(R)        = l(R);
x0          = max([B*x-c; 0]);
l1          = [0; l];
u1          = [x0; u];
x1          = [x0; x];
p1          = 0;
errorcode   = 0;
[x1,y,f,errorcode1] = proj06(a1,B1,c,p1,l1,u1,x1,errorcode);
x           = x1(2:n+1);
xx          = x1(1);
if abs(xx) > tol
   errorcode = 4;                   % feasible domain empty
end
