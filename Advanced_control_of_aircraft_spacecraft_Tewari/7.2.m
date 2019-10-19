% Calling program for solving the 2PBVP for optimal general
% rotational maneuver by collocation method using MATLAB's
% intrinsic code 'bvp4c.m'.
% Requires 'genrotode.m' and 'genrotbc.m'.
% (c) 2009 Ashish Tewari
% dy/dx=f(y,x); a<=x<=b
% y(x=a), y(x=b): Boundary conditions
% y(1:3,1)=omega (rad/s) (Angular velocity vector)
% y(4:7,1)=q_1,q_2,q_3,q_4 (Quaternion)
% y(8:14,1)=lambda (Lagrange multipliers vector)
global tf; tf=70; % Terminal time (s)
global J; J=diag([1000 1500 2000]); % Moms. of inertia (kg-m^2)
global wi; wi=[0 0 0]'; % Initial ang. velocity (rad/s)
global wf; wf=[0.03 -0.02 0.01]'; % Final ang. velocity (rad/s)
global Qi; Qi=[0 0 0 1]'; % Initial quaternion
global Qf; Qf=[0.1 -0.2 0.3 sqrt(1-.1^2-.2^2-.3^2)]';% Final quaternion
global R; R=inv(J); % Control cost coefficients
% Collocation points & initial guess follow:
solinit = bvpinit(linspace(0,tf,5),[wf; Qf; zeros(7,1)]);
% 2PBVP Solution by collocation method:
options=bvpset('Nmax',100);
sol = bvp4c(@genrotode,@genrotbc,solinit,options);
x = linspace(0,tf); % Time vector (s)
y = deval(sol,x); % Solution state vector
plot(x,y(1:3,:)),xlabel('Time (s)'),ylabel('\omega (rad/s)')
figure
plot(x,y(4:7,:)),xlabel('Time (s)'),ylabel('q')
figure
u=-0.5*inv(R)*inv(J)*y(8:10,:);
plot(x,u),xlabel('Time (s)'),ylabel('u (N-m)')
