function [x,z,f,errorcode] = dlqp_h(A,a,C,c);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% f(x) = x'*A*x/2 - a'*x = Min!,
% h(x) = C*x + c;
% INPUT A symm. positive definite (n,n)-matrix,
%       a n-column vector,
%       C (p,n)-matrix, c p-column vector
% p <= n, !! rank(C) = p NECESSARY !!!
% It suffices that A is positive definite on the Kernel of C 
% OUTPUT x optimal solution,
%        z optimal Lagrange-multiplier for h
%        f optimal value of objective function
% -- Parameter ------------------
tol  = 1.0E-7;
% -------------------------------
[p,n] = size(C); errorcode = 0;
x     = A\a;
D     = [A, C.';C, zeros(p,p)];
h1    = A*x - a;
h2    = C*x + c;
u     = D\[h1; h2];
d     = u(1:n); z  = u(n+1:n+p);
x     = x - d;  h  = C*x + c;
minh  = min(abs(h));
%CONDEST_D = condest(D)
if minh > tol
  disp(' problem unsolvable ');
   errorcode  = 1;
end;
f =  x.'*A*x/2 - a.'*x;
