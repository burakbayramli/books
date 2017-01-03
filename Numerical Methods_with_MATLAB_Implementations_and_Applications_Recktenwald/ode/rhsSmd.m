function dydt = rhsSmd(t,y,flag,zeta,omegan,a0)
% rhsSmd  Right-hand sides of coupled ODEs for a spring-mass-damper system
%
% Synopis:  dydt = rhsSmd(t,y,flag,zeta,omegan,a0)
%
% Input:    t      = time, the independent variable
%           y      = vector (length 2) of dependent variables
%                    y(1) = displacement and y(2) = velocity
%           flag   = dummy argument for compatibility with ode45
%           zeta   = damping ratio (dimensionless)
%           omegan = natural frequency (rad/s)
%           a0     = input force per unit mass
%
% Output:   dydt = column vector of dy(i)/dt values

if t<=0, fonm = 0.0;
else,    fonm = a0;    %  Force/mass (acceleration)
end

dydt = [ y(2);  fonm - 2*zeta*omegan*y(2) - omegan*omegan*y(1)];
