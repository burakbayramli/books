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
imethod=input('Choice of the scheme (1=Lax-Wendroff, 2 MacCormack, 3 Roe)\n');
if(imethod<3)
   D  =input('Artificial viscosity coefficient 0<= D <= 10]\n');
   Ddx=D*dx;
end
   
switch imethod
%=================================================   Lax-Wendroff
case 1
   txt1=['Lax-Wendroff (D=' num2str(D) ')'] ;
   fprintf('Lax-Wendroff scheme\n');   
while(t< tfinal)
   dt =HYP_calc_dt(w,dx,cfl);   % computation of the time step
   F = HYP_trans_w_f(w);        % computes the flux F from W
   F = F-Ddx*[zeros(3,1) diff(w,1,2)]; % adds the artificial dissipation term

       % predictor step (wtilde et Ftilde of dimension M-1)
   wtilde = 0.5*(w(:,1:M-1)+w(:,2:M))-0.5*dt/dx*(F(:,2:M)-F(:,1:M-1));
   Ftilde = HYP_trans_w_f(wtilde);
   Ftilde=Ftilde-Ddx*[diff(wtilde,1,2) zeros(3,1)];% adds artificial dissipation

       % corrector step (update of components 2:M-1)
   w(:,2:M-1)=w(:,2:M-1)-dt/dx*(Ftilde(:,2:M-1)-Ftilde(:,1:M-2));
   t=t+dt;
end

%=================================================  MacCormack 
case 2
    txt1=['MacCormack (D=' num2str(D) ')'];
    fprintf('MacCormack scheme\n');   
while(t< tfinal)
   dt =HYP_calc_dt(w,dx,cfl);   % computation of the time step
   F = HYP_trans_w_f(w);        % computes the flux F from W
   F = F-Ddx*[zeros(3,1) diff(w,1,2)]; % adds artificial dissipation
      % predictor step (wtilde et Ftilde of dimension M-1)
   wtilde = w(:,1:M-1)-dt/dx*(F(:,2:M)-F(:,1:M-1));
   Ftilde = HYP_trans_w_f(wtilde);
   Ftilde = Ftilde-Ddx*[diff(wtilde,1,2) zeros(3,1)];% adds artificial dissipation

       % corrector step (update of components 2:M-1)
       w(:,2:M-1)=0.5*(w(:,2:M-1)+wtilde(:,2:M-1))...
                 -0.5*dt/dx*(Ftilde(:,2:M-1)-Ftilde(:,1:M-2));
   t=t+dt;
end

%=================================================  Roe
 case 3
    txt1=['Roe '];
    fprintf('Roe''s scheme\n');   
while(t< tfinal)
   dt = 0.5*HYP_calc_dt(w,dx,cfl);   % computation of the time step
   Phi= HYP_flux_roe(w);             % % computes the conservative flux
     % solution at t_{n+1} (update of components 2:M-1)
    w(:,2:M-1)= w(:,2:M-1)-dt/dx*(Phi(:,2:M-1)-Phi(:,1:M-2));
   t=t+dt;
end

end
   
%=============================================
%---- plot of exact/numerical solutions
%============================================
t=0.2;
fprintf('Final time =%f\n',t);
usol=HYP_trans_w_usol(w);               % numerical solution
uex=HYP_shock_tube_exact(xx,x0,t);      % exact solution
HYP_plot_graph(t,xx,uex,usol,'Exact sol.', txt1);




