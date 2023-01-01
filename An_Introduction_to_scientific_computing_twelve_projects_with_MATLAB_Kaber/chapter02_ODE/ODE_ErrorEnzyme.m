%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Exercise 2.7
%%  Main script to verify the numerical convergence order of 
%%  EulerDelay et RungeKuttaDelay
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear
close all
global I
global td
tmax=50;  
td=4;
I=10.5;
y4=2*I; 
y1=I*(1+0.004*I^3);
y2=I;
Y0=[y1;y2;y2;y4]+rand(4,1);
fdelay='ODE_DelayEnzyme'; 
fprintf('Computes reference solution for Euler scheme, with very  fine step.\n');
fprintf('Depending on your computer, this may take several minutes......\n');
nmax=5000; 
[Te,Yefin]=ODE_EulerDelay(fdelay,tmax,nmax,Y0);
figure(1)
hold on
plot(Te,Yefin(1,:),'-b',Te,Yefin(2,:),'-g',Te,Yefin(3,:),'-r',...
     Te,Yefin(4,:),'-m')
title('Solution with Euler scheme')
xlabel('t')
ylabel('y(t)')
fprintf('Computes reference solution for Runge-Kutta scheme, with very  fine step\n');
fprintf('Depending on your computer, this may take several minutes......\n');
[Tr,Yrfin]=ODE_RungeKuttaDelay(fdelay,tmax,nmax,Y0);
figure(2)
hold on
plot(Tr,Yrfin(1,:),'-b',Tr,Yrfin(2,:),'-g',Tr,Yrfin(3,:),'-r',...
     Tr,Yrfin(4,:),'-m')
title('Solution with Runge Kutta scheme')
xlabel('t')
ylabel('y(t)')
nfin=nmax;
n=[100,200,300,500,1000,2000];
err=[];ere=[];
for nmax=n
 [Te,Ye]=ODE_EulerDelay(fdelay,tmax,nmax,Y0);
 figure(1)
 plot(Te,Ye(1,:),'--b',Te,Ye(2,:),'--g',Te,Ye(3,:),'--r',Te,Ye(4,:),'--m')
 [Tr,Yr]=ODE_RungeKuttaDelay(fdelay,tmax,nmax,Y0);
 figure(2)
 plot(Tr,Yr(1,:),'--b',Tr,Yr(2,:),'--g',Tr,Yr(3,:),'--r',Tr,Yr(4,:),'--m')
 err=[err,max(abs(Yr(:,nmax+1)-Yrfin(:,nfin+1)))];
 ere=[ere,max(abs(Ye(:,nmax+1)-Yefin(:,nfin+1)))];
 pause(1)
end
figure(3)
loglog(1.0./n,err,'-ob',1.0./n,ere,'-xr',1.0./n,100000.0./n.^4,'-g',...
       1.0./n ,1000.0./n,'-m')
legend('RK4','Euler','h^4','h',3)
title('Convergence order')
xlabel('h')
ylabel('Error')

