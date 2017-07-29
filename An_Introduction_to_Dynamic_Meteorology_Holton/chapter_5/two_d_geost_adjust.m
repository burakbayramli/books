% MATLAB file: two_d_geost_adjust.m for use with Chapter 7.
% Geostrophic adjustment section:  Revised  1/29/03.
% Finite difference model for linear shallow water model.
% Method is time iteration to steady state.
% Domain is f-plane channel. 
clear all
close all
Lx = 12000;  Ly = 12000;            % channel dimensions in km
Nx = 101; Ny = 101;                 % number of grid points in each direction
Nxl = Nx-1; Nyl = Ny-1;
xx=linspace(-Lx/2,Lx/2,Nx);         % Nx gridpoints in x   
yy=linspace( -Ly/2,Ly/2,Ny);        % Ny gridpoints in y
%convert distances to meters for calculations
[x,y]=meshgrid(xx*1000,yy*1000);    % Sets matrix for grid system    
dx = Lx/(Nx-1)*1000;                % grid distance in x  
dy=Ly/(Ny-1)*1000;                  % grid distance in y
%define constants and parameters
y00 = 0;             % y coordinate of disturbance maximum (m)
cor = 1.e-4;                        % coriolis parameter
A = -1000;                          % geopotential forcing amplitude
alpha = 1.e-7;                      % damping rate
CB2 = 30^2;                         % shallow wave speed squared
Rx = 1.0e6;  Ry = 1.0e6;            % forcing scales in x and y
phiinit = A./(1+((x./Rx).^2+(y./Ry).^2));   %initial disturbance
phi0 = phiinit;
%set initial velocity fields to zero
u0 = zeros(size(x)); u = u0;  un = u0;
v0 = u0; v = v0;  vn = v0;
phi = phi0;  phin = phi0;
div = zeros(size(x));
%%  time integration parameters:
nsv = 0    % counter for output figures
t=0;
time_end = input('specify ending time of integration in hours time_end =  ');
time_endsec = time_end*3600;        % ending time in seconds
dt = 1800;                          % time increment in seconds
dt2 = 2*dt;  
adt = 1-alpha*dt/2; apt= 1+alpha*dt/2;
adt2 = 1-alpha*dt;  apt2 = 1+alpha*dt;
tsave = 3600;        % interval for saving to output --and for forward stepping
ntime = fix(time_endsec/dt);        % total number of time steps
nsave = fix(time_endsec/tsave);     % number of times saved to output
t = 0;;
%take forward step as first step
% Begin time looping using leapfrog method
axis square
for s=1:ntime
    [dphix, dphiy]= gradient(phi,dx,dy);  %gradient of geopotential
    [dux, duy] = gradient(u,dx,dy);
    [dvx, dvy] = gradient(v,dx,dy);
    div=dux+dvy;
    vort=dvx-duy;
    t = t + dt;
    tt = t/tsave;
    if rem(tt,60) == 0;         % forward step to damp computational mode
        un = (adt*u +dt*(cor*v-dphix))/apt;
        vn = (adt*v+dt*(-cor*u-dphiy))/apt; 
        phin = (adt*phi+dt*(-CB2*div))/apt;
    else                        % leapfrog step 
        un = (adt2*u0 +dt2*(cor*v-dphix))/apt2;
        vn = (adt2*v0+dt2*(-cor*u-dphiy))/apt2;
        phin = (adt2*phi0+dt2*(-CB2*div))/apt2;  
    end
    %update all fields
    u0 = u;  v0 = v;  phi0 = phi;
    u = un;  v = vn;  phi = phin;
    if rem(tt,1) == 0;
        nsv = nsv+1;     
        time = tt*tsave/3600;     % show output  every hour
        
        figure(1)
        set(gca,'NextPlot','replacechildren')
        pcolor(x/1000,y/1000,vort/1.e-5)  % x, y, converted to km for graph 
        shading interp
        colorbar('v')
        caxis([-1 1])
        str1 = ['time = ' num2str(t/3600) '  hours'];
        text(-5000, -5000, str1,'Fontsize', 12)
        hold on
        % Reduces number of grid points for plotting velocity vectors
        for i=1:Nyl/2
            for j=1:Nxl/2
                xd(i,j)=x(2*i,2*j);
                yd(i,j)=y(2*i,2*j);
                uxd(i,j)=u(2*i,2*j);
                vyd(i,j)=v(2*i,2*j);
            end
        end   
        quiver(xd/1000,yd/1000,uxd,vyd,'k')
        axis([-Lx/2 Lx/2 -Ly/2  Ly/2  ])
        title('velocity (arrows),  vorticity (10^-^5 s^-^1) (color)')
        xlabel('x (km)'), ylabel('y (km)')
        H= gcf;
        M(:,nsv) = getframe(H);
        hold off
    end
end
movie(H,M,2)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



