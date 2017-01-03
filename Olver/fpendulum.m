function yp = fpendulum(t,y)
%
%  Forced pendulum
%
%   See also PENDULUM, DPENDULUM, FDPENDULUM

yp = [y(2); - sin(y(1)) + 2*cos(t)];

