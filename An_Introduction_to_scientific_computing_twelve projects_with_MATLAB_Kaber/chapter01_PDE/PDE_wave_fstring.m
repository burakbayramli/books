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
% Finite length vibrating string
%    initial conditions   
%           u(x,0)=wave_fstring_in(x)
%       du/dt(x,0)=0
%     boundary conditions
%       u(0,t)=u(l,t)=0
%====================================================

close all; clear all;   

ll=1;           % length of the vibrating string
c=2;            % convection velocity
taut=2*ll/c;    % time period


nx=50;           % number of points for the space discretization 
dx=ll/nx;        % discretization step
xx=[0:nx]*dx;    % abscissas of computational points

nt=125;           % number of integration points for a time period
dt=taut/nt;       % time step
sigma=c*dt/dx;    % CFL condition: sigma<1
fprintf('CFL condition: sigma=%f \n',sigma);

% initial conditions
ks =[1,10];       % wave-number for the wave phi_k
as =[1,0.25];     % amplitude   for the wave phi_k

time=0;
u0 =PDE_wave_fstring_in(xx,ks,as); % set the initial condition          
time=time+dt;
u1 =u0;u2=u1;

sigma2=sigma*sigma;
coeff =2*(1-sigma2);

figure('Position',0.75*get(0,'Screensize'));
% time integration -> during 2 periods 
% the vector u0 corresponds to u^{n-1}, u1 to u^{n} and u2 to u^{n+1}
for n=2:nt
   time=time+dt;
   
   % centered scheme
   % we compute only the components 2,....,nx
   u2(2:nx)=-u0(2:nx)+coeff*u1(2:nx)+sigma2*(u1(1:nx-1)+u1(3:nx+1));
   
   % plot of the solution (every 25 tile steps)
   if(mod(n,25)==0)
      uex=PDE_wave_fstring_exact(xx,time,c,ks,as);    % exact solution
      plot(xx,uex,'r-',xx,u2,'b-o','LineWidth',2,'MarkerSize',10)
      set(gca,'XTick',0:.2:1,'FontSize',24);   
      title(['time=',num2str(time),' CFL=' num2str(sigma)])
      legend('Exact sol.','Num sol.');xlabel('x');ylabel('u');grid on
                  fprintf('Press enter to continue\n');pause;
   end
   
   % update the solution vectors
   u0=u1;u1=u2;
end
