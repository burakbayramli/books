%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lax-Friedrichs method to solve 1-D Euler equations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Following the ideas of:
% 1. E.F. Toro, Riemann Solvers and Numerical Methods for Fluid Dynamics 
%    Manchester U.K., Springer Editorial, 2nd Ed., 1999. Chapert 11.
% [2] Randall J. Leveque, Finite Volume Method for Hyperbolic Problems.,
%     Cambridge University Press. 2nd Ed., 2004. Chapter 4.
%
% This code solves the Sod's shock tube problem 
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
% coded by Manuel Diaz, 2012.12.25
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear; %close all; clc;

%% Parameters
CFL     = 0.5;	% CFL number
tEnd    = 0.15;	% Final time
nE      = 100;  % Number of cells/Elements
n       = 5;	% Number of degrees of freedom
IC      = 01;	% 12 IC cases are available
plot_fig= 1;

% Ratio of specific heats for ideal di-atomic gas
gamma=(n+2)/n;

% Discretize spatial domain
a=0; b=1; dx=(b-a)/nE; nx=nE+1; x=linspace(a,b,nx);

% Set IC
[rho0,u0,p0,~,~] = Euler_IC1d(x,IC);
E0 = p0./((gamma-1)*rho0)+0.5*u0.^2;  % Total Energy
a0 = sqrt(gamma*p0./rho0);            % Speed of sound

% Discretize time domain
dt=CFL*dx/max(abs(u0+a0));  % using the system's largest eigenvalue
t = 0:dt:tEnd; 

% Exact solution
[xe,rhoe,ue,pe,ee,te,Me,se] = ...
   EulerExact(rho0(1),u0(1),p0(1),rho0(nx),u0(nx),p0(nx),tEnd,n);

%% Solver Loop
% Load initial condition
rho=rho0; u=u0; p=p0; E=E0; it=0;

for tsteps=t
    % iteration counter
    it=it+1;
    
    % define vectors q & F for every x(i)
    q=[rho; rho.*u; rho.*E];
    F=[rho.*u; rho.*u.^2+p; u.*(rho.*E+p)];
    
    % update q matrix and flow parameters
    q(:,2:nx-1) = 0.5*(q(:,3:nx) + q(:,1:nx-2))...
                    -dt/(2*dx)*(F(:,3:nx) - F(:,1:nx-2));
    % compute flow properties
    rho=q(1,:); u=q(2,:)./rho; E=q(3,:)./rho; p=(gamma-1)*rho.*(E-0.5*u.^2);
    
    % Plot figure
    if rem(it,10) == 0
        if plot_fig == 1;
            subplot(2,2,1); plot(x,rho,'.b');
            subplot(2,2,2); plot(x,u,'.m'); 
            subplot(2,2,3); plot(x,p,'.k'); 
            subplot(2,2,4); plot(x,E,'.r');
        end
	drawnow
    end
end

% Calculation of flow parameters
a = sqrt(gamma*p./rho);
M = u./a;
p_ref = 101325;	% Reference air pressure (N/m^2)
rho_ref= 1.225;	% Reference air density (kg/m^3)
s = 1/(gamma-1)*(log(p/p_ref)+gamma*log(rho_ref./rho)); 
                % Entropy w.r.t reference condition
ss = log(p./rho.^gamma);
                % Dimensionless Entropy
Q = rho.*u;     % Mass Flow rate per unit area
e = p./((gamma-1)*rho); % internal Energy

%% Final plot
offset=0.05;
s1=subplot(2,3,1); plot(x,rho,'or',xe,rhoe,'k'); xlabel('x(m)'); ylabel('Density (kg/m^3)');
s2=subplot(2,3,2); plot(x,u,'or',xe,ue,'k'); xlabel('x(m)'); ylabel('Velocity (m/s)');
s3=subplot(2,3,3); plot(x,p,'or',xe,pe,'k'); xlabel('x(m)'); ylabel('Pressure (Pa)');
s4=subplot(2,3,4); plot(x,ss,'or',xe,se,'k'); xlabel('x(m)'); ylabel('Entropy/R gas');
s5=subplot(2,3,5); plot(x,M,'or',xe,Me,'k'); xlabel('x(m)'); ylabel('Mach number');
s6=subplot(2,3,6); plot(x,e,'or',xe,ee,'k'); xlabel('x(m)'); ylabel('Internal Energy (kg/m^2s)');
title(s1,'Lax-Friedrichs Euler Solver');
