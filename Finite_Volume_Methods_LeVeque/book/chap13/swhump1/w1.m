function q = w1(data)
% Riemann invariant w1 for shallow water equations

grav = 1;
h = data(:,1);
u = data(:,2) ./ data(:,1);
q = u + 2*sqrt(grav*h);
