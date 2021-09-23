%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         Solving 2-D Euler system of equations with 5th order
%          Weighted Essentially Non-Oscilaroty (MOL-WENO5-HLL)
%
%       dq_i/dt + df_i/dx = 0, for x \in [a,b]^2 and i =1,...,D
%
%           coded by Manuel A. Diaz, manuel.ade'at'gmail.com 
%            Institute of Applied Mechanics, NTU, 2012.08.25
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% coded by Manuel A. Diaz, 2012.12.27. Last modif: 29.04.2016.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ref: C.-W. Shu, High order weighted essentially non-oscillatory schemes
% for convection dominated problems, SIAM Review, 51:82-126, (2009). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Notes: 
% 1. A fully conservative finite volume implementation of the method of
% lines (MOL) using WENO5 associated with SSP-RK33 time integration method. 
% 2. Sharpenning of contact discontinuities is NOT implemented here.

clear; %close all; clc;
global gamma

%% Parameters
CFL     = 0.60;	  % CFL number;
tFinal	= 0.038;  % Final time;
nx      = 400;    % Number of cells;
gamma   = 1.4;    % Ratio of specific heats for ideal di-atomic gas;
fluxMth ='HLLC';  % ROE, LF, LLF, AUSM, HLLE, HLLC;
reconMth='WENO5'; % WENO5, WENO7, Poly5, Poly7;
plotFig = true;   % Plot evolution

% Discretize spatial domain
Lx=1; dx=Lx/nx; xc=dx/2:dx:Lx;

% Set IC
[r0,u0,p0] = Euler_CWblastwave_IC1d(xc);
E0 = p0./((gamma-1))+0.5*r0.*u0.^2;  % Total Energy density
a0 = sqrt(gamma*p0./r0);   % Speed of sound
Q0=[r0; r0.*u0; E0];   % vec. of conserved properties

% Load reference solution
load('CWblastwaveRef.mat'); xe=x; re=r; pe=p; ue=u; ee = p./((gamma-1)*r);

% Set q-array & adjust grid for ghost cells
switch reconMth
    case {'WENO5','Poly5'}, R=3; nx=nx+2*R; in=R+1:nx-R;
	case {'WENO7','Poly7'}, R=4; nx=nx+2*R; in=R+1:nx-R;
end        
q0=zeros(3,nx); q0(:,in)=Q0;

% Initial time step
lambda0=max(abs(u0)+a0); dt0=CFL*dx/lambda0;

% Select Solver
solver = 2;
switch solver
    case 1, FV_EE1d = @FV_WENO_EE1d; % The component-wise solver
    case 2, FV_EE1d = @FV_WENO_charWise_EE1d; % The characteristic-wise solver
end

%% Solver Loop

% Load IC
q=q0; t=0; it=0; dt=dt0; lambda=lambda0;

while t<tFinal
    % iteration local time 
    if t+dt>tFinal; dt=tFinal-t; end; t=t+dt;
    
    % RK Initial step
    qo = q;
    
    % 1st stage
    L=FV_WENO_EE1d(q,lambda,nx,dx,fluxMth,reconMth,'CWblastwave'); q=qo-dt*L;

    % 2nd Stage
    L=FV_WENO_EE1d(q,lambda,nx,dx,fluxMth,reconMth,'CWblastwave'); q=0.75*qo+0.25*(q-dt*L);

    % 3rd stage
    L=FV_WENO_EE1d(q,lambda,nx,dx,fluxMth,reconMth,'CWblastwave'); q=(qo+2*(q-dt*L))/3;

    % compute flow properties
    r=q(1,:); u=q(2,:)./r; E=q(3,:)./r; p=(gamma-1)*r.*(E-0.5*u.^2); a=sqrt(gamma*p./r);
    
    % Update dt and time
    lambda=max(abs(u)+a); dt=CFL*dx/lambda;
    
    % Update iteration counter
	it=it+1;
    
    % Plot figure
    if plotFig && rem(it,10) == 0
        subplot(2,2,1); plot(xc,r(in),'.b');
        subplot(2,2,2); plot(xc,u(in),'.m');
        subplot(2,2,3); plot(xc,p(in),'.k');
        subplot(2,2,4); plot(xc,E(in),'.r');
        drawnow
    end
end

% Remove ghost cells
q=q(:,in); nx=nx-2*R; 

% compute flow properties
r=q(1,:); u=q(2,:)./r; E=q(3,:)./r; p=(gamma-1)*r.*(E-0.5*u.^2); e=p./((gamma-1)*r);

%% PostProcess

% Plots results
figure(1);
s1=subplot(2,2,1); plot(xc,r,'ro',xe,re,'-k'); xlabel('x'); ylabel('\rho, density'); 
legend(['FV-',reconMth,'-',fluxMth],'Exact'); legend boxoff;
s2=subplot(2,2,2); plot(xc,u,'ro',xe,ue,'-k'); xlabel('x'); ylabel('u, velocity'); 
s3=subplot(2,2,3); plot(xc,p,'ro',xe,pe,'-k'); xlabel('x'); ylabel('p, pressure');
s4=subplot(2,2,4); plot(xc,e,'ro',xe,ee,'-k'); xlabel('x'); ylabel('E, energy');
title(s1,['FV-',reconMth,'-',fluxMth,' Euler Eqns.']); title(s2,['time t=',num2str(t),'[s]']);