%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Exercise 2.6
%%  Main script to integrate the delayed system
%%  U'(t)=G(t,U(t),U(t-td)), U:R->R^4,   
%%        G:[0,+infinity[xR^4xR^4 -> R^4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
close all
clear
global I
global td
tmax=160; 
% definition of initial condition: 
td=4; % delay
I=10;
y4=2*I; 
y1=I*(1+0.004*I^3);
y2=I;
Y0=[y1;y2;2*y2;2*y4]+rand(4,1);
fdelay='ODE_DelayEnzyme';
pref='ODE_';
scheme='RungeKuttaDelay';    
%scheme='EulerDelay'; 
projscheme=strcat(pref,scheme);
nmax=2000;
[Te,Yefin]=feval(projscheme,fdelay,tmax,nmax,Y0);

figure(1)
hold on
plot(Te,Yefin(1,:),'-b',Te,Yefin(2,:),'-g',Te,Yefin(3,:),'-r',...
        Te,Yefin(4,:),'-m')
title(strcat('Solution with scheme : ', scheme))
xlabel('t')
ylabel('y(t)')
legend('y_1','y_2','y_3','y_4',0)


figure(2)
hold on
plot(Yefin(1,:),Yefin(2,:),'-g',Yefin(1,:),Yefin(3,:),'-r',...
      Yefin(1,:),Yefin(4,:),'-m')
title(strcat('Trajectories with scheme : ', scheme))
xlabel('y_1')
ylabel('y_i')
legend('y_2','y_3','y_4',0)

