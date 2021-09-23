%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                Solving 1-D Euler system of equations with 
%              2nd-order Hybrid Numerical schemes, namely with
%
%          Tangent Hyperbola for INterface Capturing (THINC) and
%         Monotonic Upwind Scheme for Conservation Laws (MUSCL).
%
%                   dq/dt + df(q)/dx = 0, for x in [a,b] 
%
%           coded by Manuel A. Diaz, manuel.ade'at'gmail.com 
%            Institute of Applied Mechanics, NHRI, 2018.06.20
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
% [1] Deng, Xi, et al. "High fidelity discontinuity-resolving
%     reconstruction for compressible multiphase flows with moving 
%     interfaces." Journal of Computational Physics (2018).  
% [2] Deng, Xi, Bin Xie, and Feng Xiao. "Some practical versions of
%     boundary variation diminishing (BVD) algorithm." arXiv preprint
%     arXiv:1708.01148 (2017). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Notes: 
% 1. Although the scheme is formally second order a SSP-RK33 integration 
%    method is required to improve its stability. 
% 2. As a step forward from the HLLC method used in [1], I have coded the
%    classical flux techniques such as: 
%      - the ROE approximate flux, 
%      - the Rusanov and Lax Friedrichs flux, 
%      - the AUSM+ flux, 
%      - the HLLE and HLLC flux functions. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear; %close all; clc;
global gamma

%% Parameters
CFL     = 0.50;	% CFL number
tFinal	= 0.25;	% Final time
nE      = 200;  % Number of cells/Elements
n       = 5;	% Number of degrees of freedom in the gas
IC      = 01;	% 10 IC cases are available
method  = 2;    % 1:MUSCL, 2:MUSCL-THINC-BVD
limiter ='MM';  % MC, MM, VA.
fluxMth ='HLLC'; % LF, ROE, RUS, AUSM, HLLE, HLLC.
plot_fig= true;

% Ratio of specific heats for ideal di-atomic gas
gamma=(n+2)/n;

% Discretize spatial domain
a=0; b=1; dx=(b-a)/nE; nx=nE+1; xc=linspace(a,b,nx);

% Set IC
[r0,u0,p0] = Euler_IC1d(xc,IC);
E0 = p0./((gamma-1)*r0)+0.5*u0.^2;  % Total Energy
a0 = sqrt(gamma*p0./r0);            % Speed of sound

% Exact solution
[xe,re,ue,pe,ee,te,Me,se] = ...
   EulerExact(r0(1),u0(1),p0(1),r0(nx),u0(nx),p0(nx),tFinal,n);
Ee = pe./((gamma-1)*re)+0.5*ue.^2;

% Set q-array & adjust grid for ghost cells
nx=nx+2; q0=[r0; r0.*u0; r0.*E0]; zero=[0;0;0]; q0=[zero,q0,zero];

% Boundary Conditions in ghost cells
q0(:,1)=q0(:,2); q0(:,nx)=q0(:,nx-1);   % Natural BCs

% initial time step
lambda0=max(abs(u0)+a0); dt0=CFL*dx/lambda0;  % using the system's largest eigenvalue

% Load initial condition
q=q0; it=0; dt=dt0; t=0; lambda=lambda0;

%% Solver Loop
switch method
    case 1 % MUSCL
        Method = 'MUSCL';
        while t<tFinal
            % Compute primary properties
            r=q(1,:); u=q(2,:)./r; E=q(3,:)./r; p=(gamma-1)*r.*(E-0.5*u.^2); a=sqrt(gamma*p./r);
            if min(p)<0; error('negative pressure found!'); end

            % Update dt
            lambda=max(abs(u)+a); dt=CFL*dx/lambda; if t+dt>tFinal; dt=tFinal-t; end

            % RK Initial step
            qo = q;

            % 1st stage
            L=MUSCL_EulerRes1d(lambda,q,nx,dx,limiter,fluxMth);	q=qo-dt*L;
            q(:,1)=q(:,2); q(:,nx)=q(:,nx-1); % Neumann BCs

            % 2nd Stage
            L=MUSCL_EulerRes1d(lambda,q,nx,dx,limiter,fluxMth);	q=0.75*qo+0.25*(q-dt*L);
            q(:,1)=q(:,2); q(:,nx)=q(:,nx-1); % Neumann BCs

            % 3rd stage
            L=MUSCL_EulerRes1d(lambda,q,nx,dx,limiter,fluxMth);	q=(qo+2*(q-dt*L))/3;
            q(:,1)=q(:,2); q(:,nx)=q(:,nx-1); % Neumann BCs

            % Update time and iteration counter
            t=t+dt; it=it+1;

            % Plot figure
            if rem(it,10) == 0
                if plot_fig == true
                    subplot(2,2,1); plot(xc,r(2:nx-1),'.b',xe,re);
                    subplot(2,2,2); plot(xc,u(2:nx-1),'.m',xe,ue); 
                    subplot(2,2,3); plot(xc,p(2:nx-1),'.k',xe,pe); 
                    subplot(2,2,4); plot(xc,E(2:nx-1),'.r',xe,Ee);
                end
            drawnow
            end
        end
    case 2 % MUSCL-THINC-BVD
        Method = 'MUSCL-THINC-BVD';
        while t<tFinal
            % Compute primary properties
            r=q(1,:); u=q(2,:)./r; E=q(3,:)./r; p=(gamma-1)*r.*(E-0.5*u.^2); a=sqrt(gamma*p./r);
            if min(p)<0; error('negative pressure found!'); end

            % Update dt
            lambda=max(abs(u)+a); dt=CFL*dx/lambda; if t+dt>tFinal; dt=tFinal-t; end

            % RK Initial step
            qo = q;

            % 1st stage
            L=Hybrid_EulerRes1d(lambda,q,nx,dx,limiter,fluxMth);	q=qo-dt*L;
            q(:,1)=q(:,2); q(:,nx)=qo(:,nx-1); % Neumann BCs

            % 2nd Stage
            L=Hybrid_EulerRes1d(lambda,q,nx,dx,limiter,fluxMth);	q=0.75*qo+0.25*(q-dt*L);
            q(:,1)=q(:,2); q(:,nx)=qo(:,nx-1); % Neumann BCs

            % 3rd stage
            L=Hybrid_EulerRes1d(lambda,q,nx,dx,limiter,fluxMth);	q=(qo+2*(q-dt*L))/3;
            q(:,1)=q(:,2); q(:,nx)=q(:,nx-1); % Neumann BCs

            % Update time and iteration counter
            t=t+dt; it=it+1;

            % Plot figure
            if rem(it,10) == 0
                if plot_fig == true
                    subplot(2,2,1); plot(xc,r(2:nx-1),'.b',xe,re);
                    subplot(2,2,2); plot(xc,u(2:nx-1),'.m',xe,ue); 
                    subplot(2,2,3); plot(xc,p(2:nx-1),'.k',xe,pe); 
                    subplot(2,2,4); plot(xc,E(2:nx-1),'.r',xe,Ee);
                end
            drawnow
            end
        end
    otherwise
        error('Method not listed!');
end

% Compute primary properties
r=q(1,:); u=q(2,:)./r; E=q(3,:)./r; p=(gamma-1)*r.*(E-0.5*u.^2);
if min(p)<0; error('negative pressure found!'); end

% Calculation of flow parameters
a = sqrt(gamma*p./r); M = u./a; % Mach number [-]
p_ref = 101325;           % Reference air pressure (N/m^2)
r_ref= 1.225;             % Reference air density (kg/m^3)
s_ref = 1/(gamma-1)*(log(p/p_ref)+gamma*log(r_ref./r)); 
                          % Entropy w.r.t reference condition
s = log(p./r.^gamma);     % Dimensionless Entropy
Q = r.*u;                 % Mass Flow rate per unit area
e = p./((gamma-1)*r);     % internal Energy

%% Final plot
offset=0.05; 
s1=subplot(2,3,1); plot(xc,r(2:nx-1),'or',xe,re,'k'); xlabel('x(m)'); ylabel('Density (kg/m^3)');
s2=subplot(2,3,2); plot(xc,u(2:nx-1),'or',xe,ue,'k'); xlabel('x(m)'); ylabel('Velocity (m/s)');
s3=subplot(2,3,3); plot(xc,p(2:nx-1),'or',xe,pe,'k'); xlabel('x(m)'); ylabel('Pressure (Pa)');
s4=subplot(2,3,4); plot(xc,s(2:nx-1),'or',xe,se,'k'); xlabel('x(m)'); ylabel('Entropy/R gas');
s5=subplot(2,3,5); plot(xc,M(2:nx-1),'or',xe,Me,'k'); xlabel('x(m)'); ylabel('Mach number');
s6=subplot(2,3,6); plot(xc,e(2:nx-1),'or',xe,ee,'k'); xlabel('x(m)'); ylabel('Internal Energy (kg/m^2s)');
title(s1,[Method,' Euler Solver']);