%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%===============================================
% Numerical resolution of the shock tube problem
%===============================================
clear all;format long e;
close all;
%============================================
%---- input data (global variables)
%============================================

global gamma aL pL rhoL aR pR rhoR dum1 dum2 dum3;

gamma=1.4;
dum1=2/(gamma+1);
dum2=(gamma-1)/(gamma+1);
dum3=(gamma-1)/2;

%----------------- driven section of the tube
rhoL= 8;
pL  =10/gamma ;
aL=sqrt(gamma*pL/rhoL);

%----------------- working section of the tube
rhoR=1;
pR=1/gamma;
aR=1;

%=============================================
%---- space discretization
%============================================
M  = 81; 
dx = 1./(M-1);
xx = [0:M-1]*dx;
x0 = 0.5;
ip = find(xx >= x0);   % working section
in = find(xx  < x0);   % driven section

%=============================================
%---- initialization
%============================================
usol = zeros(3,M);   % vector (rho, U, p)

usol(1,ip) =rhoR; usol(1,in)=rhoL;
usol(2,:)  =0;
usol(3,ip) =pR; usol(3,in)=pL;

w = zeros(3,M);              % vector W=(rho, rho U, E)
w = HYP_trans_usol_w(usol);  % computation of W from usol

%=============================================
%---- numerical solution: centered schemes
% only grid points 2,..., M-1 are computed
%============================================
tfinal=0.2;
t=0.;cfl=0.95;
   
txt1=['Roe '];
fprintf('Roe''s scheme\n');   
while(t< tfinal)
  dt = 0.5*HYP_calc_dt(w,dx,cfl);   % computation of the time step
  Phi= HYP_flux_roe(w);             % % computes the conservative flux
		    % solution at t_{n+1} (update of components 2:M-1)
  w(:,2:M-1)= w(:,2:M-1)-dt/dx*(Phi(:,2:M-1)-Phi(:,1:M-2));
  t=t+dt;

end
   
%=============================================
%---- plot of exact/numerical solutions
%============================================
t=0.2;
fprintf('Final time =%f\n',t);
usol=HYP_trans_w_usol(w);               % numerical solution

plot(xx,usol);

print -dpng /tmp/out1.png
