% MATLAB file:  forced_equatorial_mode2.m   for use with Chapter 11
% Revised 12/19/02
% Finite difference model for linear equatorial shallow water model.
% Given a transient thermal forcing computes wave response.
% Method is finite differences in time and spa.
% Domain is equatorial beta-plane channel periodic in x.
clear all
close all
Lx = 40000;  Ly = 8000;             % channel dimensions in km
Nx = 61; Ny = 41;                   % number of grid points in each direction
Nxl = Nx-1; Nyl = Ny-1;
xx=linspace(-Lx/2,Lx/2,Nx);         % Nx gridpoints in x   
yy=linspace( -Ly/2,Ly/2,Ny);        % Ny gridpoints in y
%convert distances to meters for calculations
[x,y] = meshgrid(xx*1000,yy*1000);  % Sets matrix for grid system    
dx = Lx/(Nx-1)*1000;                % grid distance in x  
dy = Ly/(Ny-1)*1000;                % grid distance in y
%define constants and parameters
y00 = 0;                            % y coordinate of heating maximum (m)
rad = 6.37e6;                       % radius of earth
omega = 7.292e-5;                   % earth angular velocity
beta = 2*omega/rad;                 % beta effect
A = 2.e-3;                          % forcing amplitude
alpha = 1.e-6;                      % damping rate
CB2 = 18^2;                         % shallow wave speed squared
Rx = 1.e6;  Ry = 6.e5;              % forcing scales in x and y

%set initial fields to zero
u0 = zeros(size(x)); u = u0;  un = u0;
v0 = u0; v = v0;  vn = v0;
phi0 = u0; phi = phi0;  phin = phi0;
div = u0;
eps = u0;
dphix = u0; dphiy = u0;
%%  time integration parameters:
nsv = 0;                            % counter for output figures
t = 0;
time_end = input('specify ending time of integration in hours time_end =   ');
time_endsec = time_end*3600   ;     % ending time in seconds
dt = 1800;                          % time increment in seconds
tau = 5*24*3600;                    % forcing period
dt2 = 2*dt;  
adt = 1-alpha*dt;
adt2 = 1-alpha*dt2;
tsave = 3600*6;     % interval for saving to output --and for forward stepping
ntime = fix(time_endsec/dt);        % total number of time steps
nsave = fix(time_endsec/tsave);     % number of times saved to output

% take forward step as first step
% Begin time looping using leapfrog method
for s=1:ntime
    % define the transient thermal forcing function
    heat=zeros(size(x)); 
    if t<tau
        heat = A*exp(-(x.^2/Rx^2+(y-y00).^2/Ry^2))...
            *sin(pi*t/tau);          % thermal forcing
    end
    [dphix, dphiy] = grad(phi,dx,dy); % gradient of geopotential
    t = t + dt;
    tt = t/tsave;
    if rem(tt,1) == 0;          % forward step to damp computational mode
        un = adt*u +dt*(beta*y.*v-dphix);
        vn = adt*v+dt*(-beta*y.*u-dphiy);
        [dux, duy] = grad(u,dx,dy);
        [dvx, dvy] = grad(v,dx,dy);
        div = dux+dvy;
        eps(find(div<0)) = 0;
        phin = adt*phi + dt*(-CB2*(1-eps).*div + heat);
    else                        % leapfrog step
        un = adt2*u0 + dt2*(beta*y.*v-dphix);
        vn = adt2*v0 + dt2*(-beta*y.*u-dphiy);
        [dux, duy] = grad(u,dx,dy);
        [dvx, dvy] = grad(v,dx,dy);
        div = dux+dvy;
        eps(find(div<0)) = 0;
        phin = adt2*phi0 + dt2*(-CB2*(1-eps).*div+heat);  
    end
    %update all fields
    u0 = u;  v0 = v;  phi0 = phi;
    u = un;  v = vn;  phi = phin;
    if rem(tt,1) == 0;
        
        time = tt*tsave/3600;   % show output  every tsave/3600 hours
        max(max(u));
        
        figure(1)
        subplot(2,1,1)
        pcolor(x/1.e6,y/1.e6,phi/9.8)  % x, y, converted to km for graph 
        caxis([-5 5])
        shading interp
        colorbar('h')
        hold on
        V = 0.03e-3;
        contour(x/1.e6,y/1.e6,heat,[V V],'k') 
        ylabel('y  (km \times 10^3)')
        title( 'perturbaton height in meters (color), forcing (contour)')
        hold off
        subplot(2,1,2)
        % Reduces number of grid points for plotting velocity vectors
        for i = 1:Nyl/2
            for j = 1:Nxl/2
                xd(i,j)=x(2*i,2*j);
                yd(i,j)=y(2*i,2*j);
                uxd(i,j)=u(2*i,2*j);
                vyd(i,j)=v(2*i,2*j);
            end
        end   
        quiver(xd/1.e6,yd/1.e6,uxd,vyd)
        axis([-Lx/2000 Lx/2000 -Ly/2000  Ly/2000  ])
        title('scaled velocity field')
        xlabel('x  (km \times 10^3)'), ylabel('y  (km) \times 10^3')
        hold off
    end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



