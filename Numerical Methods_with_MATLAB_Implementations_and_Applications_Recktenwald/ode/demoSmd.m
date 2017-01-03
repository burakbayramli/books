function demoSmd(zeta,omegan,tstop)
% demoSmd   Second order system of ODEs for a spring-mass-damper system
%
% Synopsis:  smdsys(zeta,omegan,tstop)
%
% Input:     zeta   = (optional) damping ratio;  Default:  zeta = 0.1
%            omegan = (optional) natural frequency;  Default: omegan = 35
%            tstop  = (optional) stopping time;  Default:  tstop = 1.5
%
% Output:    plot of displacement and velocity versus time

if nargin<1,  zeta = 0.1;   end
if nargin<2,  omegan = 35;  end
if nargin<3,  tstop = 1.5;  end

y0 = [0; 0];   a0 = 9.8;     %  Initial conditions and one g force/mass
[t,y] = ode45('rhssmd',tstop,y0,[],zeta,omegan,a0);

subplot(2,1,1);
plot(t,y(:,1));  ylabel('Displacement');  grid;
title(sprintf('zeta = %5.3f   omegan = %5.1f',zeta,omegan));
subplot(2,1,2);
plot(t,y(:,2));  xlabel('Time (s)');  ylabel('Velocity');  grid;
