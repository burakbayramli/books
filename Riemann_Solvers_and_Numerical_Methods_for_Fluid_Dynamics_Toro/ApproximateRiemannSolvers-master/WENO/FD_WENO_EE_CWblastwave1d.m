%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         Solving 1-D Euler system of equations with 5th order
%          Weighted Essentially Non-Oscilaroty (MOL-WENO5-LF)
%
%        dq_i/dt + df_i/dx = 0, for x \in [a,b] and i =1,. ..,D
%
%           coded by Manuel A. Diaz, manuel.ade'at'gmail.com 
%            Institute of Applied Mechanics, NTU, 2012.08.25
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This code solves the Sod's shock tube problem (IC=1)
%
% t=0                                 t=tEnd
% Density                             Density
%   ****************|                 *********\
%                   |                           \
%                   |                            \
%                   |                             ****|
%                   |                                 |
%                   |                                 ****|
%                   ***************                       ***********
%
% coded by Manuel A. Diaz, 2012.12.27. Last modif: 29.04.2016.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Refs: 
% [1] C.-W. Shu, High order weighted essentially non-oscillatory schemes
%     for convection dominated problems, SIAM Review, 51:82-126, (2009). 
% [2] F.-A. Kuo, M.R. Smith, C.-W. Hsieh, C.-Y. Chou and J.-S. Wu, GPU
%     acceleration for general conservation equations and its application
%     to several engineering problems, Computers and Fluids,
%     45[1]:pp.147-154,2011. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Notes: 
% 1. A fully conservative finite difference implementation of the method of
% lines (MOL) using WENO5 associated with SSP-RK33 time integration method. 
% 2. Sharpenning of contact discontinuities is NOT implemented here.

clear; close all; clc;
global gamma

%% Parameters
CFL     = 0.60;	  % CFL number;
tEnd	= 0.038;  % Final time;
nx      = 401;    % Number of nodes;
gamma   = 1.4;    % Ratio of specific heats for ideal di-atomic gas;
fsplit  ='LF';    % LF, LLF, SHLL; 
recon   ='WENO5'; % WENO5, WENO7, Poly5, Poly7;
plotFig = true;

% Discretize spatial domain
Lx=1; dx=Lx/(nx-1); xi=0:dx:Lx;

% Set IC
[r0,u0,p0] = Euler_CWblastwave_IC1d(xi);
E0 = p0./((gamma-1))+0.5*r0.*u0.^2;  % Total Energy density
a0 = sqrt(gamma*p0./r0);   % Speed of sound
Q0=[r0; r0.*u0; E0];   % vec. of conserved properties

% Load reference solution
load('CWblastwaveRef.mat'); xe=x; re=r; pe=p; ue=u; ee=p./((gamma-1)*r); 

% Set q-array & adjust grid for ghost cells
switch recon
    case {'WENO5','Poly5'}, R=3; nx=nx+2*R; in=R+1:nx-R;
	case {'WENO7','Poly7'}, R=4; nx=nx+2*R; in=R+1:nx-R;
end        
q0=zeros(3,nx); q0(:,in)=Q0;

% Discretize time domain
lambda0=max(abs(u0)+a0); dt0=CFL*dx/lambda0;  % using the system's largest eigenvalue

% Select Solver
solver = 1;
switch solver
    case 1, FD_EE1d = @FD_WENO_EE1d; % The component-wise solver
    case 2, FD_EE1d = @FD_WENO_charWise_EE1d; % The characteristic-wise solver
    case 3, FD_EE1d = @FD_WENO_EE1d_PeriodicBCs; % Solver with periodic BCs
end

%% Solver Loop

% Load initial condition
q=q0; it=0; dt=dt0; t=0; lambda=lambda0;

while t<tEnd
    % Iteration local time
    if t+dt>tEnd; dt=tEnd-t; end; t=t+dt;
 
    % RK Initial step
    qo = q;
    
    % 1st stage
    L=FD_EE1d(lambda,q,nx,dx,fsplit,recon,'CWblastwave');	q = qo-dt*L; 
    
    % 2nd Stage
    L=FD_EE1d(lambda,q,nx,dx,fsplit,recon,'CWblastwave');	q = 0.75*qo+0.25*(q-dt*L);

    % 3rd stage
    L=FD_EE1d(lambda,q,nx,dx,fsplit,recon,'CWblastwave');	q = (qo+2*(q-dt*L))/3;
   
    % compute primary properties
    r=q(1,:); u=q(2,:)./r; E=q(3,:); p=(gamma-1)*(E-0.5*r.*u.^2);
    a=sqrt(gamma*p./r); if min(p)<0; error('negative pressure found!'); end
    
    % Update time step, dt
    lambda=max(abs(u)+a); dt=CFL*dx/lambda; 
    
    % Update iteration counter
	it=it+1;
    
    % Plot figure
    if plotFig && rem(it,1) == 0
        subplot(2,2,1); plot(xi,r(in),'.b');
        subplot(2,2,2); plot(xi,u(in),'.m');
        subplot(2,2,3); plot(xi,p(in),'.k');
        subplot(2,2,4); plot(xi,E(in),'.r');
        drawnow
    end
end

% Remove ghost cells
q=q(:,in); nx=nx-2*R; 

% compute flow properties
r=q(1,:); u=q(2,:)./r; E=q(3,:)./r; p=(gamma-1)*r.*(E-0.5*u.^2);

%% Post-process

% Calculation of flow parameters
a = sqrt(gamma*p./r); M = u./a; % Mach number [-]
p_ref = 101325;           % Reference air pressure (N/m^2)
r_ref = 1.225;            % Reference air density (kg/m^3)
s_ref = 1/(gamma-1)*(log(p/p_ref)+gamma*log(r_ref./r)); 
                          % Entropy w.r.t reference condition
s = log(p./r.^gamma);     % Dimensionless Entropy
Q = r.*u;                 % Mass Flow rate per unit area
e = p./((gamma-1)*r);     % internal Energy

%% Final plot
figure(1);
s1=subplot(2,2,1); plot(xi,r,'or',xe,re,'k'); xlabel('x(m)'); ylabel('Density (kg/m^3)');
s2=subplot(2,2,2); plot(xi,u,'or',xe,ue,'k'); xlabel('x(m)'); ylabel('Velocity (m/s)');
s3=subplot(2,2,3); plot(xi,p,'or',xe,pe,'k'); xlabel('x(m)'); ylabel('Pressure (Pa)');
s6=subplot(2,2,4); plot(xi,e,'or',xe,ee,'k'); xlabel('x(m)'); ylabel('Internal Energy (kg/m^2s)');
title(s1,['FD-',recon,'-',fsplit,' Euler VFS-solver']);