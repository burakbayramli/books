function yp = fdpendulum(t,y)
%
%  Forced damped pendulum
%
%   See also PENDULUM, DPENDULUM, FPENDULUM

yp = [y(2); -.2 * y(2) - sin(y(1)) + 1.55*cos(t)];

