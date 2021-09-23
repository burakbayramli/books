%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%               basic MUSCL solver for Euler system equations
%                      by Manuel Diaz, NTU, 29.04.2015
%
%                         U_t + F(U)_x + G(U)_y = 0,
%
% MUSCL based numerical schemes extend the idea of using a linear
% piecewise approximation to each cell by using slope limited left and
% right extrapolated states. This results in the following high
% resolution, TVD discretisation scheme.   
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Refs:
%   [1] Toro, E. F., "Riemann Solvers and Numerical Methods for Fluid
%   Dynamics" Springer-Verlag, Second Edition, 1999. 
%   [2] Balsara, Dinshaw S. "A two-dimensional HLLC Riemann solver for
%   conservation laws: Application to Euler and magnetohydrodynamic flows."
%   Journal of Computational Physics 231.22 (2012): 7476-7503. 
%   [3] Einfeldt, Bernd. "On Godunov-type methods for gas dynamics." SIAM
%   Journal on Numerical Analysis 25.2 (1988): 294-318. 
%   [4] Kurganov, Alexander, and Eitan Tadmor. "Solution of two?dimensional
%   Riemann problems for gas dynamics without Riemann problem solvers."
%   Numerical Methods for Partial Differential Equations 18.5 (2002): 584-608. 
%
% coded by Manuel Diaz, 2015.05.10
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear; close all; clc;

%% Parameters
CFL     = 0.50;     % CFL number;
tEnd    = 0.25;     % Final time;
nx      = 100;      % Number of cells/Elements in x;
ny      = 100;      % Number of cells/Elements in y;
n       = 5;        % Degrees of freedom: ideal air=5, monoatomic gas=3;
IC      = 05;       % 19 IC cases are available;
fluxMth ='HLLC';	% LF, RUS, ROE, HLLE, HLLC;
limiter ='MC';      % MM, MC, VA, VL;
plot_fig= 1;        % 1:visualize evolution;

% Ratio of specific heats for ideal di-atomic gas
gamma=(n+2)/n;

% Discretize spatial domain
Lx=1; dx=Lx/nx; xc=dx/2:dx:Lx;
Ly=1; dy=Ly/ny; yc=dy/2:dy:Ly;
[x,y] = meshgrid(xc,yc);

% Set IC
[r0,u0,v0,p0] = Euler_IC2d(x,y,IC);
%[r0,u0,v0,p0] = Continuum_IC2d(x,y);
E0 = p0./((gamma-1)*r0)+0.5*(u0.^2+v0.^2);  % Total Energy
c0 = sqrt(gamma*p0./r0);                    % Speed of sound
Q0 = cat(3, r0, r0.*u0, r0.*v0, r0.*E0);    % initial state

% Set q-array & adjust grid for ghost cells
nx=nx+2; ny=ny+2; q0=zeros(ny,nx,4); q0(2:ny-1,2:nx-1,1:4)=Q0;

% Boundary Conditions in ghost cells
q0(:,1,:)=q0(:,2,:); q0(:,nx,:)=q0(:,nx-1,:);   % Natural BCs
q0(1,:,:)=q0(2,:,:); q0(ny,:,:)=q0(ny-1,:,:);   % Natural BCs

% Discretize time domain
vn = sqrt(u0.^2+v0.^2); lambda1=vn+c0; lambda2=vn-c0; 
a0 = max(abs([lambda1(:);lambda2(:)])); 
dt0=CFL*min(dx./a0,dy./a0); 

% Initialize parpool
poolobj = gcp('nocreate'); % If no pool, do not create new one.
if isempty(poolobj); parpool('local',2); end

%% Solver Loop

% Load IC
q=q0; t=dt0; it=0; dt=dt0; a=a0;

tic
while t < tEnd
    
    % RK2 1st step
    qs = q - dt*MUSCL_EulerRes2d(q,a,gamma,dx,dy,nx,ny,limiter,fluxMth);
    
    qs(:,1,:)=qs(:,2,:); qs(:,nx,:)=qs(:,nx-1,:);   % Natural BCs
    qs(1,:,:)=qs(2,:,:); qs(ny,:,:)=qs(ny-1,:,:);   % Natural BCs
    
    % RK2 2nd step / update q
    q = (q + qs - dt*MUSCL_EulerRes2d(qs,a,gamma,dx,dy,nx,ny,limiter,fluxMth))/2;
    
    q(:,1,:)=q(:,2,:); q(:,nx,:)=q(:,nx-1,:);   % Natural BCs
    q(1,:,:)=q(2,:,:); q(ny,:,:)=q(ny-1,:,:);   % Natural BCs
    
	% compute flow properties
    r=q(:,:,1); u=q(:,:,2)./r; v=q(:,:,3)./r; E=q(:,:,4)./r;
    p=(gamma-1)*r.*(E-0.5*(u.^2+v.^2)); c=sqrt(gamma*p./r);
    
    % Update dt and time
    vn=sqrt(u.^2+v.^2); lambda1=vn+c; lambda2=vn-c;
    a = max(abs([lambda1(:);lambda2(:)]));
    dt=CFL*min(dx/a,dy/a); if t+dt>tEnd; dt=tEnd-t; end
	t=t+dt; it=it+1;
    
    % Plot figure
    if rem(it,10) == 0
        if plot_fig == 1
            subplot(2,2,1); contourf(x,y,r(2:ny-1,2:nx-1)); axis('square');
            subplot(2,2,2); contourf(x,y,u(2:ny-1,2:nx-1)); axis('square');
            subplot(2,2,3); contourf(x,y,v(2:ny-1,2:nx-1)); axis('square');
            subplot(2,2,4); contourf(x,y,p(2:ny-1,2:nx-1)); axis('square');
            drawnow
        end
    end
end
cputime = toc;

% Remove ghost cells
q=q(2:ny-1,2:nx-1,1:4); nx=nx-2; ny=ny-2; 

% compute flow properties
r=q(:,:,1); u=q(:,:,2)./r; v=q(:,:,3)./r; E=q(:,:,4)./r; p=(gamma-1)*r.*(E-0.5*(u.^2+v.^2));

%% Calculation of flow parameters
c = sqrt(gamma*p./r);   % Speed of sound
Mx = u./c; My = v./c; U = sqrt(u.^2+v.^2); M = U./c;
p_ref = 101325;         % Reference air pressure (N/m^2)
rho_ref= 1.225;         % Reference air density (kg/m^3)
s = 1/(gamma-1)*(log(p/p_ref)+gamma*log(rho_ref./r)); 
                        % Entropy w.r.t reference condition
ss = log(p./r.^gamma);  % Dimensionless Entropy
r_x = r.*u;             % Mass Flow rate per unit area
r_y = r.*v;             % Mass Flow rate per unit area
e = p./((gamma-1)*r);   % internal Energy

%% Final plot
offset=0.05; n=22; % contour lines
s1=subplot(2,3,1); contour(x,y,r,n); axis('square'); xlabel('x(m)'); ylabel('Density (kg/m^3)');
s2=subplot(2,3,2); contour(x,y,U,n); axis('square'); xlabel('x(m)'); ylabel('Velocity Magnitud (m/s)');
s3=subplot(2,3,3); contour(x,y,p,n); axis('square'); xlabel('x(m)'); ylabel('Pressure (Pa)');
s4=subplot(2,3,4); contour(x,y,ss,n);axis('square'); xlabel('x(m)'); ylabel('Entropy/R gas');
s5=subplot(2,3,5); contour(x,y,M,n); axis('square'); xlabel('x(m)'); ylabel('Mach number');
s6=subplot(2,3,6); contour(x,y,e,n); axis('square'); xlabel('x(m)'); ylabel('Internal Energy (kg/m^2s)');
title(s1,['MUSCL Euler ',fluxMth,' 2-D solver']);