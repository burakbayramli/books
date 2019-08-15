function [x,errorcode] = proj04(a,B,c,p);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% projection method in tableau form
% problem: max(a'*x, B*x <= c, b^ix = c^i, i = 1:p)
% phase 1: computation of feasible point
% B (m,n)-mMatrix, a,y row vectors, c,x column vectors
% ASSUMPTION:
%         rank(B(1:p,:)) = p
%         rank(B) = n
% OUTPUT:
%        x feasible point (if exists)
%        errorcode = 3: max. step number reached
%        errorcode = 4: feasible domain empty
% REMARK:
%         IB = A(x)
%         IN = N(x) complement of A(x) in [1:m]
% ---------------------------------------------------------
% uses proj03.m
% ---------------------------------------------------------
%
tol   = 1000*eps;
[m,n] = size(B); a1 = [-1, zeros(1,n)]; ff = [-ones(m,1)];
if p > 0
   e = ones(1,p); a1 = [-1, e*B(1:p,:)];
   ff = [zeros(p,1); -ones(m-p,1)]; ss = sign([c(1:p);zeros(m-p,1)]);
   M  = find(ss == - 1); c(M) = -c(M);
   B(M,:) = -B(M,:);
end
B1 = [-1  zeros(1,n); ff B]; c1 = [0; c];
x0 = max([- c(p+1:m);0]); x = [x0; zeros(n,1)];
p1 = 0; errorcode = 0;
[x1,z,f,errorcode] = proj03(a1,B1,c1,x,p1,errorcode);
x = x1(2:n+1); xx = x1(1);
if abs(xx) >= tol
   errorcode  = 4;                   % feasible domain empty
end
