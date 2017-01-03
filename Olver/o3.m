function yp = o3(t,y)
%
%  nonlinear pendulum
%
%   See also DPENDULUM, FPENDULUM, FDPENDULUM

yp = [y(1); .1 * y(2) + y(3); -y(2) + .1*y(3)];

