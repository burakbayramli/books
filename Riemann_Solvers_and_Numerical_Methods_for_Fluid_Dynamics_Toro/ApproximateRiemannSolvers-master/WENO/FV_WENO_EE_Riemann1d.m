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
CFL     = 0.55;	  % CFL number;
tFinal	= 0.12;	  % Final time;
nx      = 200;    % Number of cells;
gamma   = 1.4;    % Ratio of specific heats for ideal di-atomic gas;
IC      = 01;	  % 10 IC cases are available;
fluxMth ='LF';    % ROE, LF, LLF, AUSM, HLLE, HLLC;
reconMth='WENO5'; % WENO5, WENO7, Poly5, Poly7;
plotFig = true;   % Plot evolution

% Discretize spatial domain
Lx=1; dx=Lx/nx; xc=dx/2:dx:Lx;

% Set IC
[r0,u0,p0] = Euler_Riemann_IC1d(xc,IC);
E0 = p0./((gamma-1))+0.5*r0.*u0.^2;  % Total Energy density
a0 = sqrt(gamma*p0./r0);  % Speed of sound
Q0 = [r0; r0.*u0; E0];  % vec. of conserved properties

% Exact solution
[xe,re,ue,pe,ee,te,Me,se] = ...
   EulerExact(r0(1),u0(1),p0(1),r0(nx),u0(nx),p0(nx),tFinal);

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
    case 2, FV_EE1d = @FV_WENO_EE1d_charWiseRecon; % The characteristic-wise solver
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
    L=FV_EE1d(q,lambda,nx,dx,fluxMth,reconMth,'Riemann'); q=qo-dt*L;

    % 2nd Stage
    L=FV_EE1d(q,lambda,nx,dx,fluxMth,reconMth,'Riemann'); q=0.75*qo+0.25*(q-dt*L);

    % 3rd stage
    L=FV_EE1d(q,lambda,nx,dx,fluxMth,reconMth,'Riemann'); q=(qo+2*(q-dt*L))/3;

    % compute flow properties
    r=q(1,:); u=q(2,:)./r; E=q(3,:); p=(gamma-1)*(E-0.5*r.*u.^2); 
    a=sqrt(gamma*p./r); if min(p)<0; error('negative pressure found!'); end
    
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
r=q(1,:); u=q(2,:)./r; E=q(3,:); p=(gamma-1)*(E-0.5*r.*u.^2);

%% PostProcess

% Calculation of flow parameters
a = sqrt(gamma*p./r); M = u./a; % Mach number [-]
p_ref = 101325;           % Reference air pressure (N/m^2)
r_ref = 1.225;            % Reference air density (kg/m^3)
s_ref = 1/(gamma-1)*(log(p/p_ref)+gamma*log(r_ref./r)); 
                          % Entropy w.r.t reference condition
s = log(p./r.^gamma);     % Dimensionless Entropy
Q = r.*u;                 % Mass Flow rate per unit area
e = p./((gamma-1)*r);     % internal Energy

% Plots results
figure(1);
s1=subplot(2,3,1); plot(xc,r,'or',xe,re,'k'); xlabel('x(m)'); ylabel('Density (kg/m^3)');
s2=subplot(2,3,2); plot(xc,u,'or',xe,ue,'k'); xlabel('x(m)'); ylabel('Velocity (m/s)');
s3=subplot(2,3,3); plot(xc,p,'or',xe,pe,'k'); xlabel('x(m)'); ylabel('Pressure (Pa)');
s4=subplot(2,3,4); plot(xc,s,'or',xe,se,'k'); xlabel('x(m)'); ylabel('Entropy/R gas');
s5=subplot(2,3,5); plot(xc,M,'or',xe,Me,'k'); xlabel('x(m)'); ylabel('Mach number');
s6=subplot(2,3,6); plot(xc,e,'or',xe,ee,'k'); xlabel('x(m)'); ylabel('Internal Energy (kg/m^2s)');
title(s1,['FV-',reconMth,'-',fluxMth,' Euler Eqns.']); title(s2,['time t=',num2str(t),'[s]']);