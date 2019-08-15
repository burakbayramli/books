function [x,z,f,errorcode] = dlqp_h(A,a,C,c,Pardlqp);
% f(x) = x'*A*x/2 - a'*x = Min!,
% h(x) = C*x + c;
% INPUT A symm. positive definite (n,n)-Matrix,
%       a n-Spaltenvektor,
%       C (p,n)-Matrix, c p-Spaltenvektor
% p <= n, C RANGMAXIMAL;
% Es genuegt, wenn A auf dem Kern von C positiv definit ist
% OUTPUT x optimale Loesung,
%        z optimaler Lagrange-Multiplikator fuer h
%        f optimaler Wert der Zielfunktion
% -- Parameter ------------------
tol  = Pardlqp(1); Eeps = Pardlqp(2);
% -------------------------------
[p,n] = size(C); errorcode = 0;
x     = A\a;
D     = [A, C.';C, Eeps*eye(p)];
h1    = A*x - a;
h2    = C*x + c;
u     = D\[h1; h2];
d     = u(1:n); z  = u(n+1:n+p);
x     = x - d;  h  = C*x + c;
minh  = min(abs(h));
%CONDEST_D = condest(D);
if minh > tol
  disp(' DLQP_H unloesbar');
   errorcode  = 1;
end;
f =  x.'*A*x/2 - a.'*x;
