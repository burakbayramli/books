function yp = pendulum(t,y)
%
%  nonlinear pendulum
%
%   See also DPENDULUM, FPENDULUM, FDPENDULUM

yp = [y(2);-sin(y(1))];

