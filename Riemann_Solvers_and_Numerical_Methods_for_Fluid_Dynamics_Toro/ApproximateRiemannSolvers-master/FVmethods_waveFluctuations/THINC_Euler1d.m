%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%               basic THINC-BVD solver for Euler system equations
%                      by Manuel Diaz, NHRI, 20.06.2018
%
%                             U_t + F(U)_x = 0,
%
% THINC schemes approximate the values of a cell average by means of a
% hyperbolic tangent reconstruction. However, the new move in this mix 
% is the incorporation of the boundary-value-diminishing (BVD) algorithm. 
% Here, two THINC schemes are reconstructed. Namely, a smooth and a fully
% discontinuous reconstruction. Then, the BVD algorithm is used to choose 
% the reconstruction that provides the lowest boundary differences.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Refs:
% [1] Toro, E. F., "Riemann Solvers and Numerical Methods for Fluid
%     Dynamics" Springer-Verlag, Second Edition, 1999.
% [2] Deng, Xi, et al. "Limiter-free discontinuity-capturing scheme for 
%     compressible gas dynamics with reactive fronts." C & F (2018).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOTE:
% Currently this code is not working. I'm trying to contact the fisrt
% author in [2] to clear some concepts. In the mean time, if anyone 
% familiar with this implementation can spoot the problem, I'll be
% eternally in debt with you ;) Happy coding!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear; %clc; close all;
global gamma

%% Parameters
cfl     = 0.50;	% CFL number
tEnd    = 0.15;	% Final time
nx      = 200;  % Number of cells/Elements
n       = 5;	% Number of degrees of freedom in the gas
IC      = 01;	% ~12 Initial value problems are available
fluxMth ='HLLC'; % LF, ROE, RUS, AUSM, HLLE, HLLC.
plot_fig= 1;

% Ratio of specific heats for ideal di-atomic gas
gamma=(n+2)/n;

% Discretize spatial domain
Lx=1; dx=Lx/nx; xc=dx/2:dx:Lx;

% Set IC
[r0,u0,p0] = Euler_IC1d(xc,IC);
E0 = p0/(gamma-1)+0.5*r0.*u0.^2;  % Total Energy
a0 = sqrt(gamma*p0./r0);            % Speed of sound

% Exact solution
[xe,re,ue,pe,ee,te,Me,se] = ...
   EulerExact(r0(1),u0(1),p0(1),r0(nx),u0(nx),p0(nx),tEnd,n);
Ee = pe/(gamma-1)+0.5*re.*ue.^2;

% Set q-array & adjust grid for ghost cells
nx=nx+2; q0=[r0; r0.*u0; E0]; zero=[0;0;0]; q0=[zero,q0,zero];

% Boundary Conditions in ghost cells
q0(:,1)=q0(:,2); q0(:,nx)=q0(:,nx-1);   % Natural BCs

% Initial time step
lambda0=abs(u0)+a0; dt0=cfl*dx/max(lambda0(:));

% Load IC
q=q0; t=0; it=0; dt=dt0; lambda=lambda0;

%% Solver Loop
tic
while t < tEnd
    
    % RK Initial step
    qo = q;
    
    % 1st step
    L=THINC_EulerRes1d_Fluctuations(q,dx,nx);     q=qo-dt*L; 
    q(:,1)=q(:,2); q(:,nx)=q(:,nx-1);   % Natural BCs
    
    % 2nd step 
    L=THINC_EulerRes1d_Fluctuations(q,dx,nx);     q=0.75*qo+0.25*(q-dt*L);
    q(:,1)=q(:,2); q(:,nx)=q(:,nx-1);   % Natural BCs
    
    % 2nd step 
    L=THINC_EulerRes1d_Fluctuations(q,dx,nx);     q=(qo+2*(q-dt*L))/3;
    q(:,1)=q(:,2); q(:,nx)=q(:,nx-1);   % Natural BCs
    
    % compute flow properties
    r=q(1,:); u=q(2,:)./r; E=q(3,:); p=(gamma-1)*(E-0.5*r.*u.^2); a=sqrt(gamma*p./r); 
    
    % Update dt and time
    lambda=abs(u)+a; dt=cfl*dx/max(lambda(:));
    if t+dt>tEnd; dt=tEnd-t; end
	t=t+dt; it=it+1;
    
    % Plot figure
    if rem(it,10) == 0
        if plot_fig == 1
            subplot(2,2,1); plot(xc,r(2:nx-1),'.b',xe,re);
            subplot(2,2,2); plot(xc,u(2:nx-1),'.m',xe,ue); 
            subplot(2,2,3); plot(xc,p(2:nx-1),'.k',xe,pe); 
            subplot(2,2,4); plot(xc,E(2:nx-1),'.r',xe,Ee);
            drawnow
        end
    end
end
cputime = toc;

% Remove ghost cells
q=q(:,2:nx-1); nx=nx-2; 

% compute flow properties
r=q(1,:); u=q(2,:)./r; E=q(3,:); p=(gamma-1)*(E-0.5*r.*u.^2); e = p./((gamma-1)*r);

% Plots results
figure(1);
subplot(2,2,1); plot(xc,r,'ro',xe,re,'-k'); xlabel('x'); ylabel('\rho'); legend(['THINC-',fluxMth],'Exact'); 
title('SSP-RK3 THINC Euler Eqns.')
subplot(2,2,2); plot(xc,u,'ro',xe,ue,'-k'); xlabel('x'); ylabel('u'); %legend(['MUSCL-',fluxMth],'Exact');
subplot(2,2,3); plot(xc,p,'ro',xe,pe,'-k'); xlabel('x'); ylabel('p'); %legend(['MUSCL-',fluxMth],'Exact');
subplot(2,2,4); plot(xc,E,'ro',xe,Ee,'-k'); xlabel('x'); ylabel('E'); %legend(['MUSCL-',fluxMth],'Exact');