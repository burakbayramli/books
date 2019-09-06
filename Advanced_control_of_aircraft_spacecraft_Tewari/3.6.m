%Calling program for solving the 2PBVP for optimal aircraft
%rotational maneuver by collocation method using MATLABs
%intrinsic code 
%Requires 'airrotode.m' and 'airrotbc.m'.
%(c) 2009 Ashish Tewari
%dy/dx=f(y,x); a<=x<=b
%y(x=a), y(x=b): Boundary conditions
%y(1:3,1)=omega (rad/s) (Angular velocity vector)
%y(4:7,1)=q_1,q_2,q_3,q_4 (Quaternion)
%y(8:14,1)=lambda (Lagrange multipliers vector)
global tf; tf=2; % Terminal time (s)
global J; J=[15000 0 500;0 36000 0;500 0 50000];%(kg-m^2)
global wi; wi=[1 0.3 -0.02]'; % Initial ang. velocity (rad/s)
global Qi; Qi=[0 0 0 1]'; % Initial quaternion
global R; R=inv(J); % Control cost coefficients
% Collocation points & initial guess follow:
solinit = bvpinit(linspace(0,tf,5),[zeros(3,1);
[0.125 0 0 sqrt(1-.125^2)]'; zeros(7,1)]);
% 2PBVP Solution by collocation method:
options=bvpset('Nmax',100);
sol = bvp4c(@airrotode,@airrotbc,solinit,options);
x = linspace(0,tf); % Time vector (s)
y = deval(sol,x); % Solution state vector
plot(x,y(1:3,:)),xlabel('Time (s)'),ylabel('\omega (rad/s)')
figure

plot(x,y(4:7,:)),xlabel('Time (s)'),ylabel('q,q_4')
figure
jx=J(1,1);jy=J(2,2);jz=J(3,3);jxz=J(1,3);
Jxz=J(1,3);
D=jx*jz-jxz^2;
Fu=[jz/D 0 -jxz/D; 0 1/jy 0; -jxz/D 0 jx/D];
u=-0.5*inv(Rbar)*Fu'*y(8:10,:);
plot(x,u/1000),xlabel('Time (s)'),ylabel('u (x1000 N-m)')
