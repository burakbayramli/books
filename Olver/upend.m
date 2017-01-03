function f = upend(t,u)

% undamped pendulum

f = [u(2);-sin(u(1))];
