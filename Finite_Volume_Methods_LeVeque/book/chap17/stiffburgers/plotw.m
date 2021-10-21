
beta = 0.8;
tau = 1.0;
x = -25:.1:25;

% traveling wave solution:
xi = x/tau;
if xi<0
    w = exp(xi) ./ (1 + exp(xi));
  else
    w = 1 ./ (exp(-xi) + 1);
  end

plot(x,w)
axis([-25 25 -.2 1.4])

query

psi = w.*(1-w).*(w-beta)/tau;
plot(x,psi)


