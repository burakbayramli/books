% MATLAB file  trajectory_3.m
% Trajectory problem for Chapter 3
% This version can be run as a movie to show propagation of streamlines and
% parcel trajectories for an initial cluster of N parcels (N=12 in example)
% Horizontal motion with zonally propagating sinusoidal wave streamfunction
% including a SW-NE tilt of the troughs and ridges and zonal jet structure
% Space coordinates are scaled to units of  km
% 3rd order accuracy time differencing used
% Cyclic boundary conditions in zonal direction (parcels are reentrant)
%
close all
clear all
% First define the grid points on which fields are computed:
% (Distances are in units of 1000 km).
xx=linspace(-3000,3000,30);         % 30 gridpoints in x   
yy=linspace( -1000,1000,20);        % 20 gridpoints in y
[x,y]=meshgrid(xx*1000,yy*1000);    % Sets matrix for grid system in x and y in meters
disp('Trajectories are plotted for particles initially clustered near x=0 km')
ystart=1000*input('give a starting y coordinate in km, try 750, 0, or -750: y= ')
ub = input('give a mean zonal wind in m/s (try  12)  ' )
up = input('give a perturbation horizontal wind in m/s (try  15) ')
cp = input('give a wave phase speed in m/s (try 10)   ')
t=0;
dt = 1800;
dt12 = dt/12;
cor=1.e-4;
k=2*pi/3.e6;
m=pi/2.e6;
runtime = input('integration time in days  ')
time = runtime*24.*3600.;
nsv = 0;
tsave =3;            % time interval for data save (hours)
nsave = fix(runtime*24/tsave);  %number of frames saved
N = 12;              % number of parcels in cluster
X=zeros(N,1);        % vector for particle positions
for n=1:N
    X(n,:)= i*ystart+ 2.e4*exp(i*n*2*pi/N);
end
xprimn= zeros(size(X));
xprim1=xprimn;
xprim2=xprimn;
% vector xprimn has components of dX/dt
figure(1)
phi = -cor*ub/m*(sin(m*y)+0.5*sin(2*m*y))...
    +cor*up/k*sin(k*(x-cp*t)-m*y).*cos(m*y);
top = max(max(phi))/9.8;
v1= [0:top/4:top];
contour(x/1000,y/1000,phi/9.8,v1,'k-');
hold on
contour(x/1000,y/1000,phi/9.8, [0 0],'r-');
v2=[-top:top/4:0];
contour(x/1000,y/1000,phi/9.8,v2,'k--');
hold off
for t= 0:dt:time
    phi = -cor*ub/m*(sin(m*y)+0.5*sin(2*m*y))...
        +cor*up/k*sin(k*(x-cp*t)-m*y).*cos(m*y);
    Xn = X +dt12*(23*xprimn -16*xprim1 +5*xprim2);
    X = Xn;
    xprim2 = xprim1;
    xprim1 = xprimn;
    
    xprimn = ub*(cos(m*imag(X))+1.0*cos(2*m*imag(X)))...
        +up*m/k*sin(k*(real(X)-cp*t)-m*imag(X)).*sin(m*imag(X))...
        +up*m/k*cos(k*(real(X)-cp*t)-m*imag(X)).*cos(m*imag(X))...
        +i* (up*cos(k*(real(X)-cp*t)-m*imag(X)).*cos(m*imag(X)));
    for n=1:N
        if real(X(n)) > 3.e6
            X(n)= -6.e6+X(n);
        end
        if real(X(n)) < -3.e6
            X(n) = 6.e6+X(n);
        end
    end
    
    if mod(t,tsave*3600)==0
        
        mnpt=mean(X)/1000;
        set(gca, 'NextPlot', 'replacechildren')
        nsv = nsv +1;
        timsv(nsv)=t/(24*3600);
        stdev(nsv)=std(imag(X))/1000;
        ymean(nsv)=imag(mnpt);
        point(nsv)=mnpt;
        contour(x/1000,y/1000,phi/9.8,v1,'k-');
        hold on
        contour(x/1000,y/1000,phi/9.8,v2,'k--');
        
        plot(real(X)/1000,imag(X)/1000, 'bo', 'MarkerSize',4)
        plot(real(mnpt),imag(mnpt),'r*','MarkerSize',8) 
        xlabel('x (km)'),  ylabel('y (km)')
        title('Streamfunction and particle position')
        hold  off
        %make movie
        H = gca;
        M(:,nsv) = getframe(H);
    end
    t = t +dt;
end
movie(M)

figure(2)
plot(timsv,stdev)
ylabel('km'), xlabel('time (days)')
title('standard deviation of y position')
figure(3)
plot(timsv,ymean)
ylabel('km'), xlabel('time (days)'), title('mean y position')
figure(4)
plot(point,'*-')
ylabel('y position in km'), xlabel('x position in km');
title('Mean parcel positions at 6 hour intervals')
