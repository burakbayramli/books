%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%             basic 2-D WENO solver for Euler system equations
%                      by Manuel Diaz, NTU, 29.04.2015
%
%                         U_t + F(U)_x + G(U)_y = 0,
%
%           coded by Manuel A. Diaz, manuel.ade'at'gmail.com 
%            Institute of Applied Mechanics, NTU, 2012.12.27
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% coded by Manuel A. Diaz, 2012.12.27. Last modif: 29.04.2016.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear; %close all; clc;
global gamma 

%% Parameters
CFL     = 0.60;    % CFL number
tEnd    = 0.30;    % Final time
nx      = 201;     % Number of cells/Elements in x
ny      = 201;     % Number of cells/Elements in y
n       = 5;       % Degrees of freedom: ideal air=5, monoatomic gas=3.
IC      = 03;      % 19 IC cases are available
fspltMth='LF';     % LF, LLF.
reconMth='WENO5';  % WENO5, WENO7, Poly5, Poly7;
plotFig = false;   % Visualize evolution of domain

% Ratio of specific heats for ideal di-atomic gas
gamma=(n+2)/n;

% Discretize spatial domain
Lx=1; dx=Lx/(nx-1); xc=0:dx:Lx;
Ly=1; dy=Ly/(ny-1); yc=0:dy:Ly;
[x,y] = meshgrid(xc,yc);

% Set IC
[r0,u0,v0,p0] = Euler_Riemann_IC2d(x,y,IC);
E0 = p0./(gamma-1)+0.5*r0.*(u0.^2+v0.^2);  % Total Energy
c0 = sqrt(gamma*p0./r0);                   % Speed of sound
Q0 = cat(3, r0, r0.*u0, r0.*v0, E0);       % initial state

% Set q-array & adjust grid for ghost cells
switch reconMth
    case {'WENO5','Poly5'}, R=3; nx=nx+2*R; ny=ny+2*R; in=R+1:ny-R; jn=R+1:nx-R;
	case {'WENO7','Poly7'}, R=4; nx=nx+2*R; ny=ny+2*R; in=R+1:ny-R; jn=R+1:nx-R;
end        
q0=zeros(ny,nx,4); q0(in,jn,:)=Q0;

% Discretize time domain
vn = sqrt(u0.^2+v0.^2); lambda1=vn+c0; lambda2=vn-c0; 
a0 = max(abs([lambda1(:);lambda2(:)])); 
dt0=CFL*min(dx./a0,dy./a0); 

% Initialize parpool
% poolobj = gcp('nocreate'); % If no pool, do not create new one.
% if isempty(poolobj); parpool('local',4); end

% Configure figure 
if plotFig
    figure(1);
    subplot(2,2,1); [~,h1]=contourf(x,y,r0); axis('square'); xlabel('x'); ylabel('y'); title('\rho');
    subplot(2,2,2); [~,h2]=contourf(x,y,u0); axis('square'); xlabel('x'); ylabel('y'); title('u_x');
    subplot(2,2,3); [~,h3]=contourf(x,y,v0); axis('square'); xlabel('x'); ylabel('y'); title('u_y');
    subplot(2,2,4); [~,h4]=contourf(x,y,p0); axis('square'); xlabel('x'); ylabel('y'); title('p');
end

% Select Solver
solver = 2;
switch solver
    case 1, FD_EE2d = @FD_WENO_EE2d; % Component-wise reconstruction
    case 2, FD_EE2d = @FD_WENO_EE2d_CharactRecon; % Characteristic-wise reconstruction
end

%% Solver Loop

% Load IC
q=q0; t=dt0; it=0; dt=dt0; a=a0;

tic
while t < tEnd
    % Interation local time
    if t+dt>tEnd; dt=tEnd-t; end; t=t+dt;
    
    % RK Initial step
    qo = q;
    
    % 1st stage
    L=FD_EE2d(q,a,nx,ny,dx,dy,t,fspltMth,reconMth,'Riemann'); q=qo-dt*L;
    
    % 2nd Stage
    L=FD_EE2d(q,a,nx,ny,dx,dy,t,fspltMth,reconMth,'Riemann'); q=0.75*qo+0.25*(q-dt*L);
    
    % 3rd stage
    L=FD_EE2d(q,a,nx,ny,dx,dy,t,fspltMth,reconMth,'Riemann'); q=(qo+2*(q-dt*L))/3;
    
	% Compute flow properties
    r=q(:,:,1); u=q(:,:,2)./r; v=q(:,:,3)./r; E=q(:,:,4); p=(gamma-1)*(E-0.5*r.*(u.^2+v.^2)); 
    c=sqrt(gamma*p./r);
    
    % Update dt and time
    vn=sqrt(u.^2+v.^2); lambda1=vn+c; lambda2=vn-c;
    a = max(abs([lambda1(:);lambda2(:)])); dt=CFL*min(dx/a,dy/a); 
    
    % update iteration counter 
    it=it+1;
    
    % Plot figure
    if plotFig && rem(it,10) == 0
        set(h1,'ZData',r(in,jn));
        set(h2,'ZData',u(in,jn));
        set(h3,'ZData',v(in,jn));
        set(h4,'ZData',p(in,jn));
        drawnow
    end
end
cputime = toc; disp(['CPU time: ',num2str(cputime),' s']);

% Remove ghost cells
q=q(in,jn,:); nx=nx-2*R; ny=ny-2*R; 

% compute flow properties
r=q(:,:,1); u=q(:,:,2)./r; v=q(:,:,3)./r; E=q(:,:,4); p=(gamma-1)*(E-0.5*r.*(u.^2+v.^2));

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
n=30; % contour lines
s1=subplot(2,3,1); contour(x,y,r,n); axis('square'); xlabel('x(m)'); ylabel('Density (kg/m^3)');
s2=subplot(2,3,2); contour(x,y,U,n); axis('square'); xlabel('x(m)'); ylabel('Velocity Magnitud (m/s)');
s3=subplot(2,3,3); contour(x,y,p,n); axis('square'); xlabel('x(m)'); ylabel('Pressure (Pa)');
s4=subplot(2,3,4); contour(x,y,ss,n);axis('square'); xlabel('x(m)'); ylabel('Entropy/R gas');
s5=subplot(2,3,5); contour(x,y,M,n); axis('square'); xlabel('x(m)'); ylabel('Mach number');
s6=subplot(2,3,6); contour(x,y,e,n); axis('square'); xlabel('x(m)'); ylabel('Internal Energy (kg/m^2s)');
title(s1,['FD ',reconMth,'-',fspltMth,' Riemann Config ',num2str(IC)]); title(s2,['time t=',num2str(t),'[s]']);