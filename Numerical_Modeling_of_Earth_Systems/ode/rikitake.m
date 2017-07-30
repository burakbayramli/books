function rikitake
%
% solve the Rikitake coupled disk dynamo analog equations
%
% parameters  (periodic)
%a = 4;m = 2;
% parameters  (chaotic)
a = 10;m = 2;
% initial conditions
x0(1) = -6; x0(2) = 2.0;x0(3) = 2;
% time range
trange = [0 200];
%
% solve ODE
%
options=odeset('RelTol',1e-5);
[t,x] = ode45(@dxdt,trange,x0,options,m,a); % could use different solver
%
% plot
%
figure(1);clf
subplot(2,1,1);
% y vs. time
plot(t,x(:,1));
xlabel('time');ylabel('y')
% phase space
subplot(2,1,2);
plot3(x(:,1),x(:,2),x(:,3));
xlabel('x');ylabel('y');zlabel('z');

%
% ODEs
%
function dxdt = dxdt(t,x,m,a)
 dxdt = [ 
   -m*x(1) +  x(3)*x(2)
   -m*x(2) + (x(3) - a)*x(1)
   1.0 - x(1)*x(2) 
   ];
