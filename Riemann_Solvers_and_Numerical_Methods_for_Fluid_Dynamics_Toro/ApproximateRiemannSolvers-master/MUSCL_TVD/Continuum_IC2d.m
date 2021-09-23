function [r_0,u_0,v_0,p_0] = Continuum_IC2d(x,y)
% Build simple continuum initial conditions for 2d problems

% 2-d Sin wave, in [0,2]x[0,2] discrete domain with periodic BCs.
% Run up to time: t=4.0.

r_0 = 1+0.2*sin(pi*(x+y));
u_0 = 1.0*ones(size(x));
v_0 =-0.5*ones(size(x));
p_0 = 1.0*ones(size(x));
