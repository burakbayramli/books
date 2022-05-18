%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Exercise 2.2
%%  Integration of a nonlinear system of  2 ode's
%%  
%%  U(t)=fun(t,U), U:R->RxR,   fun:[0,+infinity[xRxR -> RxR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global A
global B
% The right hand side of the system is defined in function fun2.m and uses
% global parameters A and B
fun='ODE_fun2' ; 
% stable case
A=1;
B=0.9;
%
U0=[2;1]; % initial condition  
t0=0;     % initial time
t1=10;   % final time
[timeS1,solS1]=ode45(fun,[t0,t1],U0);

close all
figure(1);
hold on 
plot(timeS1,solS1(:,1),'-',timeS1,solS1(:,2),'--')
legend('X','Y')
xlabel('t')
ylabel('y(t), x(t)')
title('X and Y versus time')

figure(2); 
hold on
plot(solS1(:,1),solS1(:,2))
title('Y versus X - stable case');

U0=[0.5;0.5]; % try another initial condition  
[timeS2,solS2]=ode45(fun,[t0,t1],U0);
plot(solS2(:,1),solS2(:,2),'--')
legend('[2;1]','[0.5,0.5]',0)
xlabel('x(t)')
ylabel('y(t)')
xlim([0.2,2.2])
ylim([0.2,1.2])
%%
%%
'Hit on key to continue',pause; 
% Unstable periodic case
close all
A=1;
B=3.2;
t1=30;
U0=[2;1]; % Initial condition  
[timeI1,solI1]=ode45(fun,[t0,t1],U0);

figure(1);   
plot(timeI1,solI1(:,1),'-',timeI1,solI1(:,2),'--')
legend('X','Y')
xlabel('t')
ylabel('y(t), x(t)')
title('Concentrations X and Y - unstable case')

figure(2); 
hold on
plot(solI1(:,1),solI1(:,2))
title('Y versus X  unstable case');

U0=[2;3]; % Try another initial condition  
[timeI2,solI2]=ode45(fun,[t0,t1],U0);

plot(solI2(:,1),solI2(:,2),'--g')
legend('[2;1]','[2;3]',0)
xlabel('x(t)')
ylabel('y(t)')
xlim([0.,4.5])
ylim([0.5,5.5])

