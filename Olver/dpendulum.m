function yp = dpendulum(t,y)
%
%  Damped pendulum
%
%   See also PENDULUM, FPENDULUM, FDPENDULUM

yp = [y(2); - .2 * y(2) - sin(y(1))];

