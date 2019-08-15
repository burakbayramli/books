function [A,a,B,b,x,u,C,c] = randdata(m,n)
% generates random positiv definite matrix A
% after Goldfarb/Idnani, p. 15
clc
% n number of variables
% m number of inequality constraints
p = ceil(n/3); % number of equality constraints
q = ceil(m/3); % or m/9 , AA = [1:q] active constraints in optimum
%q = m-q;
A = zeros(n,n);

%rand('state',0);
rand('state',sum(100*clock));

B = -1 + 2*rand(m,n); % matrix of constraints;
C = -1 + 2*rand(p,n); % matrix of constraints;
x = -5 + 10*rand(n,1);
c = - C*x;
s = rand(m,1); s(1:q) = 0;
u = 30*rand(m,1); u(q+1:m) = 0; % optimum dual solution

property = 100;
%while ~ismember(property,[1,2])
%   property = input('Matrix well or ill-conditioned (1/2)' );
%end
property = 2;
A = -1 + 2*rand(n,n);   % elements randomly between -1 and 1
A = A - diag(diag(A));  % diag of A is zero
S = zeros(n,1);
for I = 1:n
   S(I) = sum(abs(A(I,:)));
end
AUX1 = S + rand(n,1) + 1; AUX2 = AUX1;
AUX3 = rand(n,1);
for I = 2:n
   AUX2(I) = AUX2(I-1) + S(I) + S(I-1) + AUX3(I);
end
switch property
case 1
   A = A + diag(AUX1);
case 2    
   A = A + diag(AUX2);
end         
a = A*x - B.'*u; 
b = s - B*x; 
%Lagrangenorm  = norm(A*x - a - B.'*u)
%pause
%s
%pause
