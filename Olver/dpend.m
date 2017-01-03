function f = dpend(t,u)

% damped pendulum

f = [u(2);-.01*u(2)-sin(u(1))];
