function demoSystem
% demoSystem  Solve system of two coupled first order ODEs
%
% Synopsis:  demoSystem
%
% Input:     none
%
% Output:    Plot of solution

y0 = [0; 2];    % Initial conditions stored in a column vector
tn = 3;
%  [t,y] = ode45('rhsSys',tn,y0);        %  ode45 solution
[t,y] = odeRK4sysv('rhsSys',tn,0.1,y0);  %  odeRK4sys solution
plot(t,y(:,1),'+',t,y(:,2),'o');
xlabel('t');  ylabel('y_1  and  y_2');  legend('y_1','y_2');
