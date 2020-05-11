% Equality constrained entropy maximization
% standard newton
p = 4
n = 7
A = randn(p,n)
xhat = rand(n,1)
b = A * xhat

MAXITERS = 100;
ALPHA = 0.01;
BETA = 0.5;
NTTOL = 1e-7;
x0 = xhat
x = x0;

for iter=1:MAXITERS
  val = x'*log(x);
  grad = 1+log(x);
  hess = diag(1./x);
  sol = -[hess A'; A zeros(p,p)] \ [grad; zeros(p,1)];
  v = sol(1:n);
  fprime = grad'*v;
  if (abs(fprime) < NTTOL), break; end;
  t=1;
  while (min(x+t*v) <= 0), t = BETA*t; end;
  while ((x+t*v)'*log(x+t*v) >= val + t*ALPHA*fprime), t=BETA*t; end;
  t
  x = x + t*v;
end;

x
