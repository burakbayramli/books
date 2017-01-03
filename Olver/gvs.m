function f = gvs(t)

%  solution to  du/dt = (1 - 4*t/3)*u

f = exp(t - 2*t.^2/3);
