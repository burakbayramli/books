global g; g=9.81;% m/s^2
global T; T=30/g;% projected intercept time (s)
global RT0; RT0=[-10 -5 0.5]'; % target's initial position (m)
global VT0; VT0=[10 0 20]'; % target's initial velocity (m/s)
V0=VT0+RT0/T % interceptor's initial velocity (m/s)
%Runge-Kutta integration of interceptor's equations of motion:
[t,x]=ode45(@dragonfly,[0 3],[0 0 0 V0']);
% Calculation of instantaneous position error:
RT=RT0*ones(size(t'))+VT0*t'+.5*[0 0 -g]'*(t.^2)';
error=sqrt((x(:,1)-RT(1,:)').^2+(x(:,2)-RT(2,:)').^2+(x(:,3)-RT(3,:)').^2);
