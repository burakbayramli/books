% MATLAB file geost_adjust_1.m
% geostrophic adjustment in shallow water model for initial height disturbance
% consisting of single  cosine(kx) dependence and no dependence on y
% Student can vary Coriolis parameter of initial height disturbance
% to see influence on the response
% Note that in this example with single sinusoidal mode the decay to
% geostrophic balance is caused entirely by the specified weak damping.
% which acts on the divergent zonal flow component
disp('barotropic Rossby adjustment problem for single mode')
disp('initial condition for height in meters:  h = 10*cos(kx)  ')
lat = input('give  latitude in degrees ')
cor = 2*7.292e-5*sin(pi*lat/180)
runtime = input('integration time in days ')
time = runtime*24*3600
Lx = input('give zonal wavelength in km  ')
alph = 1.e-5
k = 2*pi/(Lx*1.e3)          % zonal wavenumber in m-1
csq = 20^2                  % shallow water speed squared csq = gH
x = [-1.e6:1.e5:1.e6];
options = [];               % place holder
% solution is saved once each hour
[t,y] = ode45('yprim_adj_1',[0:3600: time],[0 0 98],options,cor,csq,k, alph);
subplot(4,1,1), plot(t/(24*3600),y(:,1));
ylabel('u velocity (m/s)')
subplot(4,1,2), plot(t/(24*3600),y(:,2));
 ylabel( 'v velocity (m/s)')
subplot(4,1,3), plot(t/(24*3600),y(:,3)/9.8);
 ylabel('height (m)')
energy = (y(:,1).^2 + y(:,2).^2+ y(:,3).^2/csq)/2;
subplot(4,1,4), plot(t/(24*3600),energy)
xlabel('time (days)'); ylabel('energy (m^2/s^2)')

