%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        Solving 1-D Euler system of equations with WENO5 under a 
%         Finite Volume (FV) using the wave fluctuations approach
%
%        dq_i/dt + df_i/dx = 0, for x \in [a,b] and i =1,. ..,D
%
%           coded by Manuel A. Diaz, manuel.ade'at'gmail.com 
%            Institute of Applied Mechanics, NTU, 2012.12.27
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
% coded by Manuel A. Diaz, 2012.12.27, Last modif: 2018.06.20.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Refs: 
% [1] LeVeque, Randall J. Finite difference methods for ordinary and
%     partial differential equations: steady-state and time-dependent
%     problems. Vol. 98. Siam, 2007.  
% [2] Ketcheson, David I., Matteo Parsani, and Randall J. LeVeque.
%     "High-order wave propagation algorithms for hyperbolic systems."
%     SIAM Journal on Scientific Computing 35.1 (2013): A351-A377.  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOTE: (NOT completed!)
% This implementation follows the same finite-volume principles of [2],
% and is equivalent to that of sharpclaw package in Clawpack [1] solver. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear; %close all; clc;
global gamma

%% Parameters
CFL     = 0.50;	% CFL number
tFinal	= 0.15;	% Final time
nE      = 200;  % Number of cells/Elements
n       = 5;	% Number of degrees of freedom in the gas
IC      = 01;	% 10 IC cases are available
lim     = 'MM'; % MC, MM, VA.
plot_fig= 1;

% Ratio of specific heats for ideal di-atomic gas
gamma=(n+2)/n;

% Discretize spatial domain
a=0; b=1; dx=(b-a)/nE; nx=nE+1; x=linspace(a,b,nx);

% Set initial condition IC
[r0,u0,p0] = Euler_IC1d(x,IC);
E0 = p0./(gamma-1)+0.5*r0.*u0.^2;  % Total Energy 
a0 = sqrt(gamma*p0./r0);           % Speed of sound
q0=[r0; r0.*u0; E0];               % vec. of conserved properties

% Exact solution
[xe,re,ue,pe,ee,te,Me,se] = ...
   EulerExact(r0(1),u0(1),p0(1),r0(nx),u0(nx),p0(nx),tFinal,n);
Ee = pe./((gamma-1)*re);            % Internal Energy

% Initial time step
lambda0=max(abs(u0)+a0); dt0=CFL*dx/lambda0;  % using the system's largest eigenvalue

% Load initial condition
q=q0; it=0; dt=dt0; t=0; lambda=lambda0;

%% Solver Loop
while t<tFinal
 
    % RK Initial step
    qo = q;
    
    % 1st stage
    dF=WENO5_EulerRes1d_Fluctuations(q,dx,nx);	q = qo-dt*dF; 
    %q(:,1)=qo(:,3); q(:, end )=qo(:,end-2);     % Neumann BCs
    %q(:,2)=qo(:,3); q(:,end-1)=qo(:,end-2);     % Neumann BCs
    
    % 2nd Stage
    dF=WENO5_EulerRes1d_Fluctuations(q,dx,nx);	q = 0.75*qo+0.25*(q-dt*dF);
    %q(:,1)=qo(:,3); q(:, end )=qo(:,end-2);     % Neumann BCs
    %q(:,2)=qo(:,3); q(:,end-1)=qo(:,end-2);     % Neumann BCs

    % 3rd stage
    dF=WENO5_EulerRes1d_Fluctuations(q,dx,nx);	q = (qo+2*(q-dt*dF))/3;
    %q(:,1)=qo(:,3); q(:, end )=qo(:,end-2);     % Neumann BCs
    %q(:,2)=qo(:,3); q(:,end-1)=qo(:,end-2);     % Neumann BCs
   
    % compute primary properties
    r=q(1,:); u=q(2,:)./r; E=q(3,:); p=(gamma-1)*(E-0.5*r.*u.^2);
    a=sqrt(gamma*p./r); if min(p)<0; error('negative pressure found!'); end
    
    % Update dt and time
    lambda=max(abs(u)+a); dt=CFL*dx/lambda; if t+dt>tFinal; dt=tFinal-t; end
    
    % Update time and iteration counter
	t=t+dt; it=it+1;
    
    % Plot figure
    if rem(it,2) == 1
        if plot_fig == 1
            subplot(2,2,1); plot(x,r,'.b');
            subplot(2,2,2); plot(x,u,'.m'); 
            subplot(2,2,3); plot(x,p,'.k'); 
            subplot(2,2,4); plot(x,E,'.r');
        end
	drawnow
    end
end

% Compute flow properties
r=q(1,:); u=q(2,:)./r; E=q(3,:); p=(gamma-1)*(E-0.5*r.*u.^2);

% Calculation of flow parameters
a = sqrt(gamma*p./r); M = u./a; % Mach number [-]
p_ref = 101325;        	% Reference air pressure (N/m^2)
r_ref = 1.225;          % Reference air density (kg/m^3)
s_ref = 1/(gamma-1)*(log(p/p_ref)+gamma*log(r_ref./r)); 
                      	% Entropy w.r.t reference condition
s = log(p./r.^gamma);	% Dimensionless Entropy
Q = r.*u;            	% Mass Flow rate per unit area
e = p./((gamma-1)*r);	% internal Energy

%% Final plot
offset=0.05;
s1=subplot(2,3,1); plot(x,r,'or',xe,re,'k'); xlabel('x(m)'); ylabel('Density (kg/m^3)');
s2=subplot(2,3,2); plot(x,u,'or',xe,ue,'k'); xlabel('x(m)'); ylabel('Velocity (m/s)');
s3=subplot(2,3,3); plot(x,p,'or',xe,pe,'k'); xlabel('x(m)'); ylabel('Pressure (Pa)');
s4=subplot(2,3,4); plot(x,s,'or',xe,se,'k'); xlabel('x(m)'); ylabel('Entropy/R gas');
s5=subplot(2,3,5); plot(x,M,'or',xe,Me,'k'); xlabel('x(m)'); ylabel('Mach number');
s6=subplot(2,3,6); plot(x,e,'or',xe,ee,'k'); xlabel('x(m)'); ylabel('Internal Energy (kg/m^2s)');
title(s1,'WENO5-FV-HLLC wave fluctuation Euler solver');