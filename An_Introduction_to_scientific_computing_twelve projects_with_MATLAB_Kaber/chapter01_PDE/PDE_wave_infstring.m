%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
% ===========================
% solves the wave equation
%   d^2u/dt^2 -c^2 d^2u/dx^2= 0
%   for x\in [a,b] 
%=============================
% Infinite length vibrating string
%    initial conditions (periodic functions)  
%       u(x,0)=wave_infstring_u0(x)
%   du/dt(x,0)=wave_infstring_u1(x)
%====================================================

close all; clear all;   

a=0; b=1;       % computational domain bounds
c=2;            % convection velocity
taus=1;         % period in time
taut=taus/c;    % period in space

nx=50;          % number of points for the space discretization 
dx=(b-a)/nx;    % discretization step
xx=a+[0:nx]*dx; % abscissas of computational points

% trick to take into account the periodicity f(b)=f(a)
jn=[1:nx+1];          % indices j   (x_1=a, x_{nx+1}=b)
jj=jn;   jj(nx+1)=1;  % indices j  with  periodicity correction
jp=jj+1; jp(nx)  =1;  % indices j+1 with  periodicity correction
jm=jn-1; jm(1)=nx;    % indices j-1 with  periodicity correction


nt=50;            % number of integration points for a time period
dt=taut/nt;       % time step
sigma=c*dt/dx;    % CFL condition: sigma<1
fprintf('CFL condition: sigma=%f \n',sigma);

% initial conditions
time=0;
u0 =PDE_wave_infstring_u0(xx);           
time=time+dt;
u1 =u0+dt*PDE_wave_infstring_u1(xx);

sigma2=sigma*sigma;
coeff =2*(1-sigma2);

figure('Position',0.75*get(0,'Screensize'));
% time integration -> during 2 periods 
% the vector u0 corresponds to u^{n-1}, u1 to u^{n} and u2 to u^{n+1}
for n=2:2*nt
   time=time+dt;
   
   % centered scheme
   u2=-u0+coeff*u1+sigma2*(u1(jm)+u1(jp));
   
   % plot of the solution (every period)
   if(mod(n,nt)==0)
      plot(xx,PDE_wave_infstring_u0(xx),'r-',xx,u2,'b-o','LineWidth',2,'MarkerSize',10)
      set(gca,'XTick',0:.2:1,'FontSize',24);   
      title(['time=',num2str(time),' CFL=' num2str(sigma)])
      legend('Exact sol.','Num sol.');xlabel('x');ylabel('u');grid on
            fprintf('Press enter to continue\n');pause;
   end
   
   % update the solution vectors
   u0=u1;u1=u2;
end
