function xdot = dharmosc(t,x)
%
% Nonlinear dynamic model for damped harmonic oscillator
% with drifing frequency and damping time-constant
%
% INPUTS
%   x is the state vector (4x1)
% OUTPUT
%   xdot is its time-derivative
% Nominal values are 1 Hz with 5 sec time constant
% Frequency autocorrelation time constant of 20 sec.
% Damping autocorrelation time constant of 60 sec.
%
xdot    = zeros(4,1);
xdot(1) = -x(1)/x(3) + x(4)*x(2);
xdot(2) = -x(4)*x(1) - x(2)/x(3);
xdot(3) = (-x(3)+5)/60;
xdot(4) = (-x(4)+2*pi)/20;