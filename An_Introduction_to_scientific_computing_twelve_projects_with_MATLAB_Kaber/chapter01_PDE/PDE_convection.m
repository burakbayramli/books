%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
% ===========================
% solves the convection equation
%   du/dt +c du/dx= 0
%   for x\in [a,b] and
%   the initial condition  u(x,0)=conv_init_cond(x)
%   the boundary condition u(a,t)=conv_bound_cond(t)
%====================================================
% !attention c>0
%====================================================


close all; clear all;   

a=0; b=1;       % computational domain bounds
c=4;            % convection velocity
nx=40;          % number of points for the space discretization 
dx=(b-a)/nx;    % discretization step
xx=a+[0:nx]*dx; % abscissas of computational points


sigma=0.8;        % CFL condition: sigma<1
dt=sigma*dx/c;    % time step
csigma=c*dt/dx;

time=0;
u =PDE_conv_init_cond(xx); % initial condition

%  time loop
%==============
for n=1:50
   time=time+dt;
   % use a backward loop to be sure that the value
   % of u(j-1) is taken at time t_n
   for j=nx+1:-1:2    
      u(j)=(1-csigma)*u(j)+csigma*u(j-1);
   end
   u(1)=PDE_conv_bound_cond(time);  % impose the boundary condition
   % plot the results every 10 time steps
   if(mod(n,10)==0)
         % exact solution
       uex=PDE_conv_exact_sol(a,b,c,xx,time,'PDE_conv_init_cond','PDE_conv_bound_cond');
      figure('Position',0.75*get(0,'Screensize'));
      plot(xx,uex,'r-',xx,u,'b-o','LineWidth',2,'MarkerSize',10)
      xlabel('x','FontSize',24);set(gca,'XTick',0:.2:1,'FontSize',24);   
      title(['time=',num2str(time)],'FontSize',24)
      legend('Exact sol.','Numerical sol.')
   end
end
