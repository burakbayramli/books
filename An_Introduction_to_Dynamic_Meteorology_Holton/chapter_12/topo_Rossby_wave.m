% MATLAB file:  topo_rossby_wave.m
% Topographic baroclinic rossby wave in 1-d quasigeostrophic model for 
% ridge defined by the function     h(x) = -hm/(1+(x/L)^2).
% Student can vary Coriolis parameter and width of ridge (L)
% to see the influence of rotation and zonal scale on the response.
% Fast fourier transform is used to determine spectral coefficients
% for the solution.
clear
close all 
disp('topographic baroclinic Rossby wave for isolated ridge')
disp('ridge profile  h(x) = -hm/(1+(x/L)^2 ')
%  Set parameters
lat = 45;                   % central latitude of channel
cor = 2*7.2921e-5*sin(pi*lat/180);
beta = 2*7.2921e-5*cos(pi*lat/180)/6.37e6;
bv = 4.e-4;                 % buoyancy frequencyb squared
g = 9.81;
%
U = input('give mean zonal wind in m/s  ');
Lz = 30;     		        % depth in km
L = 800;    		        % zonal scale of ridge in km  
Lx = 2*pi*6.37e3*cos(pi*lat/180); % Lx is the length of the domain in km 
Ly = 8000;		            %  Ly is meridional width of domain km
H = 8000;		            % Scale height in meters
k = 2*pi/(Lx*1.e3);         % lowest zonal wavenumber in 
m =  pi/(8.0e6); 	        % meridional wavenumber
%%*********************************************
% Damping rate 'alph'  can be varied 
disp(  'damping rate per second =  ') 
r = 2.0e-6                  % damping rate per second
%%*********************************************
NL = 61;		            % number of vertical gridpoints
N = 128 ;                   % number of modes for Fourier transform
psinz = zeros(NL,N); 
psiz = zeros(NL,N);
zz = linspace(0,Lz,NL);
xx = linspace(0, Lx, N); 
deg = linspace(0,360,N);
s = [1:N];
xm = Lx/4;                  % location of maximum disturbance in km
hx = 2000./(1+((xx-xm)/L).^2);   %topographic profile
% Fourier transform hx(x) to get hn(s)
hn = fft(hx,N);
hn(1) = 0;
%  solve for streamfunction for each fourier mode
for n  =   2:N
    ns = n-1;
    Kn2 = (k*ns)^2+m^2;
    eps = r/(k*ns*U);       % damping parameter
    %  vertical wavenumber from analytic solution
    Bn = (bv/cor^2)*(beta/(U*(1+eps^2))-(Kn2+cor^2/(bv*4*H^2))...
        +i*beta*eps/(U*(1+eps^2)));
    %  Streamfunction for each Fourier mode
    psin = -(bv/cor)*hn(n)/((i*sqrt(Bn)+1/(2*H))*(1-i*eps));
    for j = 1:NL
        psinz(j,n) = psin*exp(i*sqrt(Bn)*zz(j)*1.e3 +zz(j)*1.e3/(2*H));  %vertical profile 
        psinz(j,1) = 0;
        % Potential temperature contours (approximate trajectories)
        xsi(j,n) = hn(n)*exp(i*sqrt(Bn)*zz(j)*1.e3 +zz(j)*1.e3/(2*H))...
            /(1-i*eps);   
    end 
end
for j = 1:NL
    psiz(j,:) = real(ifft(psinz(j,:),N));  % streamfunction in x,z space
    xsiz(j,:) = real(ifft(xsi(j,:),N));    % vertical displacement (meters)
    theta(j,:) = 300*exp(bv/g*zz(j)*1.e3)...
        -300*bv/g*xsiz(j,:);               % trajectories in x,z space
end
figure(1)
[x, z] = meshgrid(deg,zz);
pcolor(x,z,cor/g*psiz)
shading interp
colorbar('v')
title('geopotential height')
xlabel('longitude in degrees'), ylabel('height in km')
disp('Press any key to continue')
pause
figure(2)
cs = contour(x,z,theta,[300 320 340 360 380 400 ]);
clabel(cs)
axis([0 360 0 8])
title('potential temperature')
xlabel('longitude in degrees'), ylabel('height in km')
disp('Press any key to continue')
pause
figure(3)  
yy= linspace(0,Ly,15);
[x,y] = meshgrid(deg,yy);
[psir,y] = meshgrid(psiz(10,:),yy);
[hxy, y] = meshgrid(hx,yy);
hxy_contour = hxy.*sin(m*y*1.e3);
psi_contour = psir.*sin(m*y*1.e3)-U*y*1.e3;
subplot(2,1,1),  plot(deg,hx)
axis([0 360 0 max(hx)])
ylabel('height (m)')
title('ridge profile')
subplot(2,1,2), plot(deg,real(psiz(10,:))*cor/9.8)
axis([0 360 min(psiz(10,:)*cor/9.8)   max(psiz(10,:)*cor/9.8) ])
ylabel('height (m)'), xlabel('longitude in degrees')
title('geopotential height at z*=4.5 km')
disp('Press any key to continue')
pause
figure(4)
contour(x,y,hxy_contour, [1000 1000],'k--')
hold on
ylabel('y in km'), xlabel('longitude in degrees')
contour(x,y,psi_contour)
title('1 km topography (dashed); streamfunction at z*= 4.5 km (solid)')

