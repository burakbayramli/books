%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Exercise 2.7
%%  Main script to study the influence of the initial condition 
%%  in the delayed system
%%  U'(t)=G(t,U(t),U(t-td)), U:R->R^4,   
%%        G:[0,+infinity[xR^4xR^4 -> R^4
%%  U(0)=U0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear
close all
tmax=160;  %
global I
global td
td=4;
I=10.5;
y4=2*I; 
y1=I*(1+0.004*I^3);
y2=I;
fdelay='ODE_DelayEnzyme'; 
nmax=1000;
for k=1:10
  Y0=100*rand(4,1);  
  [Tr,Yrfin]=ODE_RungeKuttaDelay(fdelay,tmax,nmax,Y0);
  col=Y0(1:3)/100
  for i=2:4
    figure(i)
    hold on
    plot(Yrfin(1,:),Yrfin(i,:),'Color',col)
    xlabel('y_1(t)')
    title(strcat(strcat('Component y_',num2str(i)),'(t)'))      
    ylabel(strcat(strcat('y_',num2str(i)),'(t)'))  
    drawnow
  end
end

