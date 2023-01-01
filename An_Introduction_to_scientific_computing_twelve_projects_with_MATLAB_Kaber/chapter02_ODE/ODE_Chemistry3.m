%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Exercise 2.4
%%  Integration of a nonlinear system of  3 ode's
%%  
%%  U(t)=fun(t,U), U:R->RxRxR,   fun:[0,+infinity[xRxRxR -> RxRxR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear
close all
global v
%v=0.9     % stable case
v=1.3    % unstable periodic case
%v=1.52;   % unstable divergent case
fun='ODE_fun3';
U0=[1;2;1];  % initial condition
t0=0;        % initial time
t1=60;       % final time
[timeS1,solS1]=ode45(fun,[t0,t1],U0);

figure(1);   hold on
plot(timeS1,solS1(:,1),'b-',timeS1,solS1(:,2),'r--',...
     timeS1,solS1(:,3),'g-.')
legend('X','Y','Z') 
title(strcat('X Y and Z v=',num2str(v)))
xlabel('t')
ylabel('y(t), x(t),z(t)')

figure(2);  hold on
plot(solS1(:,1),solS1(:,2),'-ob')
title(strcat('Y versus X for v=',num2str(v)));
figure(3);  hold on
plot(solS1(:,1),solS1(:,3),'-ob')
title(strcat('Z versus X for v=',num2str(v)));
figure(4);  hold on
plot(solS1(:,2),solS1(:,3),'-ob')
title(strcat('Z versus  Y for v=',num2str(v)));
%
% Second integration starting from a different initial condition
%
U0=[2;2;2]; 
[timeS2,solS2]=ode45(fun,[t0,t1],U0);
figure(2);  
plot(solS2(:,1),solS2(:,2),'--xg')
figure(3); 
plot(solS2(:,1),solS2(:,3),'--xg')
figure(4); 
plot(solS2(:,2),solS2(:,3),'--xg')
legend('[1;2;1]','[2;2;2]')
xlabel('x(t)')
ylabel('y(t),z(t)')
