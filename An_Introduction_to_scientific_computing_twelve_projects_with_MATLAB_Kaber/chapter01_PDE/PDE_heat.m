%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
% ===========================
% solves the heat equation
%   du/dt -c d^2u/dx^2= 0
%   for x\in [a,b] and
%   the initial condition  u(x,0)=heat_u0(x)
%   the boundary conditions u(a,t)=us ; u(b,t)=0;
%====================================================

close all; clear all;   

a=0; b=1;       % computational domain bounds
nx=50;          % number of points for the space discretization  
dx=(b-a)/nx;    % discretization step
xx=a+[0:nx]*dx; % abscissas of computational points

nt=1200;          % number of time steps for the integration
sigma=0.5;        % CFL condition: sigma<0.5
dt=sigma*dx*dx;   % time step

% initial condition
time=0;

% choice of the computing case
irun=input('Choose the run case : finite (1) or infinite (2) domain\n');

if(irun==1)
%---------- Case of a finite domain
us=1;                    % boundary condition for  x=0
ks=[1:20];               % wave-number for the wave phi_k
as=-2/pi*(1./ks);        % amplitude   for the wave phi_k
u0=zeros(1,length(xx));  % initial condition 
u0(1)=1;                 % boundary condition for  x=0
nprint=100;
% 
elseif(irun==2)
%---------- Case of a infinite domain 
 us=0;                    % boundary condition for  x=0
 ks=[1,10];               % wave-number for the wave phi_k
 as=[1,0.25];             % amplitude   for the wave phi_k
 u0=PDE_heat_u0(xx,ks,as);    % boundary condition for  x=0
 nprint=10;
else
 fprintf('Please choose 1 or 2\n');
 return;
end

% plot of the initial condition
ymin=min(u0)-0.1*(max(u0)-min(u0));
ymax=max(u0)+0.1*(max(u0)-min(u0));
figure('Position',0.75*get(0,'Screensize'));
plot(xx,u0,'g-','LineWidth',2,'MarkerSize',10);
axis([a,b,ymin,ymax]);title('Initial condition','FontSize',24);
set(gca,'FontSize',24);  
fprintf('Press enter to continue\n');pause;

% time integration 
% the vector u0 corresponds to u^{n-1} and  u1 to u^{n}
%======================================================
    coeff =(1-2*sigma);
    u1=u0;
    ulin=(1-xx/(b-a))*us;    % linear part of the exact solution

for n=1:nt
   time=time+dt;
   
   % centered scheme (we compute only the components 2,...,nx)
   u1(2:nx)=sigma*u0(3:nx+1)+coeff*u0(2:nx)+sigma*u0(1:nx-1);
   
   % plot of the solution every nprint time steps
   if(mod(n,nprint)==0)
      clf;
      uex =PDE_heat_uex(xx,time,ks,as)+ulin;           
      plot(xx,uex,'r-',xx,u1,'o','LineWidth',2,'MarkerSize',10);
%
      if(irun==1)
        hold on; 
        plot(xx,us*(1-erf(xx/2/sqrt(time))),'g-*','LineWidth',2,'MarkerSize',10);
        legend('Exact sol.','Num sol.','erf-sol');
      else
        legend('Exact sol.','Num sol.');
      end
%
      title(['time=',num2str(time),' CFL=' num2str(sigma)],'FontSize',24);
      xlabel('x','FontSize',24);ylabel('u','FontSize',24);
      set(gca,'FontSize',24); grid on
      axis([a,b,ymin,ymax]);drawnow;
      fprintf('Press enter to continue (Ctrl-C to stop)\n');pause;

   end
   
   % update the solution vector
   u0=u1;
end
