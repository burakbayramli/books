% MATLAB file: surface_front_1.m
% Finite difference model illustrating surface frontogenesis
% uses specified wave pattern with deformation velocity fields.
% Temperature initially has linear distribution in y.
% Velocity zero at northern and southern boundaries.
clear all
close all
Lx = 3000;  Ly = 3000;              % channel dimensions in km
Nx = 65; Ny = 33;                   % number of grid points in each direction
Nxl = Nx-1; Nyl = Ny-1;
xx=linspace(-Lx/2,Lx/2,Nx);         % Nx gridpoints in x   
yy=linspace( -Ly/2,Ly/2,Ny);        % Ny gridpoints in y
%convert distances to meters for calculations
[x,y] = meshgrid(xx*1000,yy*1000);  % Sets matrix for grid system in x and y
%   *********Define the function to be contoured*********
k = 2*pi/(Lx*1000) ;                % zonal wavenumber in units of 1/ m
m = pi/(Ly*1000);                   % meridional wavenumber
dx = Lx/(Nx-1)*1000;                % grid distance in x  
dy = Ly/(Ny-1)*1000;                % grid distance in y
A = 15/k;                           % wave geopotential amplitude
% compute initial streamfunction and temperature field
psi= -A*sin(k*x).*sin(m*y);
T0 = 300-30*(y/3.e6);
[dxT,dyT] = grad(T0,dx,dy);         % gradient T for advection terms
[dxpsi,dypsi] = grad(psi,dx,dy);    % computes gradient of psi
u = -dypsi;                         % u = -d(psi)/dy
v = dxpsi;                          % v = +d(psi)/dx
%%  time integration parameters:
nsv = 0;                            % counter for output figures
t = 0;
time_end = input('specify ending time of integration in hours time_end =   ');
time_endsec = time_end*3600;        % ending time in seconds
dt = 1800;                          % time increment in seconds

tsave = 3600*3;     % interval for saving to output --and for forward stepping
ntime = fix(time_endsec/dt);         % total number of time steps
nsave = fix(time_endsec/tsave);      % number of times saved to output

%take forward step as first step
T = T0 - dt*(u.*dxT+v.*dyT);
% call to function to compute stream function and T after first step
[dxT,dyT] = grad(T,dx,dy);          % gradient T for advection terms
[dxpsi,dypsi] = grad(psi,dx,dy);    % computes gradient of psi
u = -dypsi;                         % u = -d(psi)/dy
v = dxpsi;                          % v = +d(psi)/dx
omax = 1.e-7*max(max(psi));
V2=[0.1:omax/4:omax];
V3=[-omax:omax/4:-0.1];
% Begin time looping using leapfrog method
for s=1:ntime
    t = t + dt;
    tt = t/tsave;
    
    if rem(tt,1) == 0;             % forward step to damp computational mode
        Tn(:,1:Nxl) = T(:,1:Nxl) ...
            - dt*(u(:,1:Nxl).*dxT(:,1:Nxl)+v(:,1:Nxl).*dyT(:,1:Nxl));
    else 
        Tn(:,1:Nxl) = T0(:,1:Nxl) ...
            -2*dt*(u(:,1:Nxl).*dxT(:,1:Nxl)+v(:,1:Nxl).*dyT(:,1:Nxl));
    end
    Tn(:,Nx)=Tn(:,1);
    T0 = T;                         % update old time
    T = Tn;
    
    [dxT,dyT] = grad(T,dx,dy);      % gradient T for advection terms
    [dxpsi,dypsi] = grad(psi,dx,dy);    % computes gradient of psi
    u = -dypsi;                         % u = -d(psi)/dy
    v = dxpsi;                          % v = +d(psi)/dx
    
    gradmax(s)=dyT((Ny-1)/2,(Nx-1)/2);
    tgrad(s)=t;
    str1= ['time in hours  =  ' num2str(t/3600)];
    if rem(tt,1) == 0;
        figure(1)
        pcolor(x/1000,y/1000,T)
        shading interp
        hold on 
        contour(x/1000,y/1000,psi*1.e-7,V2,'k') % x, y, converted to km for graph
        text( -1200,0,str1)
        contour(x/1000,y/1000,psi*1.e-7,V3,'k--')
        xlabel('x  (km)'), ylabel('y  (km)')
        title('T (color) psi (contours:solid positive, dashed negative)' )
        hold off
    end
end
figure(2)
plot(1.e5*dyT(:,(Nx-1)/2),yy);
ylabel('y  (km)')
xlabel('temperature gradient in K per 100 km')
