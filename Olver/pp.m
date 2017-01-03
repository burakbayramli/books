function f = pp(t,u)

% A predator-prey model

f = [2*u(1) - u(1)*u(2);- 9 * u(2)  + 3 * u(1)*u(2)];
