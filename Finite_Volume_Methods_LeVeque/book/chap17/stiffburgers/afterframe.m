axis([-5 5 -0.2 1.4])

% true solution:
setprob
xi = (x-beta*t)/tau;
if xi<0
    w = exp(xi) ./ (1 + exp(xi));
  else
    w = 1 ./ (exp(-xi) + 1);
  end

hold on
plot(x,w)
hold off

