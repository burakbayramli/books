function f = vdp(t,u)

% van der Pol oscillator

f = [u(2);- .2 * (u(1)^2 - 1) * u(2)  - u(1)];
