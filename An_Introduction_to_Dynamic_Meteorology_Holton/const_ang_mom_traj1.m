% MATLAB script: const_ang_mom_traj1.m 
% Problem M1.4   
% Script to compute constant angular momentum trajectories in spherical 
% coordinates with curvature terms included and to show time development.
% Stars mark time at one day intervals.
% Time differencing by 3rd order Adams-Bashforth method.
close all
disp('Initial longitude is zero. Specify latitude and speed when asked. ')
init_lat = input('(1) Give an initial latitude in degrees  ');
u0 = input('(2) Give a zonal wind in m/s ' );
v0 = input('(3) Give a meridional wind in m/s ');
rad= 6.37e6;
omega=7.292e-5;
lat = pi*init_lat/180.;
dt = 60;
dt12 = dt/12;
runtime = input('Specify integration time in days  ');
time = runtime*24*3600;
ind = 1;
X  = [u0 v0 0 lat];
xprimn= zeros(1,4);
% vector xprimn has components of dX/dt
xprimn(1) = (2.*omega+ind*X(1)/(rad*cos(X(4))))*sin(X(4))*X(2);
xprimn(2) = -(2.*omega+ind*X(1)/(rad*cos(X(4))))*sin(X(4))*X(1);
xprimn(3) = X(1)/(rad*cos(X(4)));
xprimn(4) = X(2)/rad;
xprim1 = xprimn;
xprim2 = xprim1;
figure(1)
axis([-180 180 -60 60])
xlabel('longitude')
ylabel('latitude')
title('constant angular momentum trajectory')
hold on

for t= 0:dt:time
    Xn = X +dt12*(23*xprimn -16*xprim1 +5*xprim2); 
    X = Xn;
    xprim2 = xprim1;
    xprim1 = xprimn;
    xprimn(1) = (2.*omega+ind*X(1)/(rad*cos(X(4))))*sin(X(4))*X(2);
    xprimn(2) = -(2.*omega+ind*X(1)/(rad*cos(X(4))))*sin(X(4))*X(1);
    xprimn(3) = X(1)/(rad*cos(X(4)));
    xprimn(4) = X(2)/rad;
    if X(3) > pi
        X(3)= -2*pi+X(3);
    end
    if X(3) < -pi
        X(3) = 2*pi+X(3);
    end
    v = X(3:4)*180./pi;
    p = plot(v(1),v(2),':','EraseMode','none', 'MarkerSize',5);
    set(p,'Xdata',v(1),'Ydata',v(2))
    if mod(t,24*3600)==0;
        plot(v(1),v(2),'*','EraseMode','none', 'MarkerSize',8);
    end
    drawnow
    t = t +dt;
end
%


