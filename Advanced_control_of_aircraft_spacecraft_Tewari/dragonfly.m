function xdot=dragonfly(t,x)
global g;
global T;
global RT0;
global VT0;
rT=RT0+VT0*t+0.5*[0 0 -g]'*t^2;
r=rT-x(1:3,1);
xdot(1:3,1)=x(4:6,1);
eps=1e-8;
% Avoiding singularity at t=T:
if abs(T-t)>eps
xdot(4:6,1)=[0 0 -g]'+r/((T-t)*(T-t+1));
else
xdot(4:6,1)=[0 0 -g]';
end
