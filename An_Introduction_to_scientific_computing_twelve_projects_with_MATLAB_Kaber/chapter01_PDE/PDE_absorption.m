%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%=========================================================
% Solves the absoption equation
%   u'(t)+ alpha*u(t)=0, alpha=constant
%    with given initial condition u(0)
% ========================
% compares to the exact sol: u(t)=exp(-alpha*t)*u(0)
%=========================================================

close all;    % close all the windows
clear all;    % clear all the variables
format long e % computation in double precision 

global alpha; % global variable
alpha=4;

t0=0;     % initial time
t1=3;     % final time
u0=1;     % initial condition

% discretization step h=1/8
np1 = 24;
tp1 = linspace(t0,t1,np1+1); % vector containing time instants t_i
sol1= PDE_EulerExp('PDE_absorp_source', u0,t0,t1,np1); % solution using explicit Euler scheme
sor1=  PDE_RKutta4('PDE_absorp_source', u0,t0,t1,np1); % solution using Runge-Kutta 4 scheme

% exact solution
sole=exp(-alpha*tp1)*u0;


% discretization step $h=1/2$
np2 = 6;
tp2 = linspace(t0,t1,np2+1); 
sol2= PDE_EulerExp('PDE_absorp_source', u0,t0,t1,np2);
sor2=  PDE_RKutta4('PDE_absorp_source', u0,t0,t1,np2); 

% graphics for Explicit Euler
figure('Position',0.75*get(0,'Screensize'));
plot(tp1,sol1,'r-.',tp2,sol2,'k:',tp1,sole,'b-','LineWidth',2);
fs2=24;set(gca,'YTick',-1:.2:1,'FontSize',fs2);set(gca,'XTick',0:.5:3)
legend('Euler h=1/8','Euler h=1/2', 'Exact sol.');
xlabel('time');ylabel('u');title('Explicit Euler')

% graphics for Runge-Kutta 4
figure('Position',0.75*get(0,'Screensize'));
plot(tp1,sor1,'r-o',tp2,sor2,'k:',tp1,sole,'b-','LineWidth',2,'MarkerSize',10);
fs2=24;set(gca,'YTick',0:.2:1,'FontSize',fs2);set(gca,'XTick',0:.5:3)
legend('RKutta4 h=1/8','RKutta4 h=1/2', 'Exact sol.');
xlabel('time');ylabel('u');title('Runge-Kutta 4')


% Stability domains

[xx,yy]=meshgrid(-3:0.01:1,-4:0.01:4); % grid of the complex plane C
zz=complex(xx,yy);                     % variable z \in C
                                       % amplification functions
gz1=1+zz;                              % Explicit Euler 
gz2=gz1+zz.*zz/2;                      % Runge-Kutta 2
gz3=gz2+(1/6+zz/24).*(zz.^3);          % Runge-Kutta 4

figure('Position',0.75*get(0,'Screensize'));
fs2=24;set(gca,'YTick',-4:1:4,'FontSize',fs2);set(gca,'XTick',-4:1:1)
contour(xx,yy,abs(gz1),[1,1])          % iso-level |G(z)|=1
axis equal;
axis([-4,1,-4,4]);grid on;hold on;
contour(xx,yy,abs(gz2),[1,1],'-.')
contour(xx,yy,abs(gz3),[1,1],':')
xlabel('x');ylabel('y');title('Stability domains')
text(-1.8,0,'\fontsize{14}Euler exp.')
text(-1.8,3.2,'\fontsize{14}RKutta 4')
text(-1.4,1.9,'\fontsize{14}RKutta 2')
