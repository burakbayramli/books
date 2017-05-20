function zdot=eoce4non(t,z);
zdot=[z(2);-0.1*z(2)-16.3*sin(z(1))];

function zdot=eoce4lin(t,z);
zdot=[z(2);-0.1*z(2)-16.3*z(1)];


clf
tspan=[0 20]; % the time span for the simulation
z0=[0.2 0 ];  % initial condition vector
[t,z]=ode23('eoce4non',tspan, z0);
plot(t,z(:,1),'b*'); % determine swinging range
hold on
[t,z]=ode23('eoce4lin',tspan, z0);
plot(t,z(:,1));
title('The linearized and the nonlinear systems with z0=[0.2 0]');
gtext('linearized model');
gtext('nonlinear model')
xlabel('time in seconds');
ylabel('theta in radians')
