% MATLAB file:  twolayer_model_3B.m
% This program extends the transient growth of neutral
% waves in the two-level model given in twolayer_model_2B to case of
% compact initial disturbance in the upper level with  Fourier modes
% all of which are shorter than the critical wavelength for stability.
% Demonstrates downstream development.
% Meridional scale with wavenumber m is included.  
% Total Streamfunctions (mean plus wave ) for the upper and 
% lower levels are plotted for a disturbance that is initially
% confined entirely to the upper layer. 
% Zonal scale can be varied to examine stability cut-off.
% Geometry is a midlatitude f-plane  (beta neglected in this version).
% (Distances are in units of km).
clear all
close all
cor = 2*7.292e-5*sin(pi/4);
beta = 0;                   % beta effect neglected when this term zero
sigma = 2.0e-6 ;            % static stability parameter
dp = 50000;		            % pressure interval in Pa
lam2 = cor^2/(sigma*dp^2);
L = 1850;                   % input('zonal scale in km  ')
Lx = 20000;                 % zonal length of domain in km
k0 = 2*pi/(L*1000);         % lowest zonal wavenumber in units of 1/ m
m = pi/6.e6;		        % meridional wavenumber in 1/m
N = 64;                     % number of modes for Fourier transform
%  define the grid points on which fields are computed:
xx=linspace(-Lx/4,3*Lx/4,N);  % 60 gridpoints in x   
yy=linspace( 0,6000,15);      % 15 gridpoints in y
[x,y]=meshgrid(xx,yy);        % Sets matrix for grid system in x and y
dk = .05*k0;

psiM0 = 1.e6;
Um = 00;			          % mean zonal wind
UT = input(' input basic state thermal wind =  ' );
xm = Lx/6;  %initial location of upper level low

% NOTE that x and y are in km for convenience in graphing
t=0;
dt = 4*3600;                  % time interval in seconds

for j = 1:24
    psiM=zeros(size(x));
    psiT=zeros(size(x));
    for n=1:9
        k = k0*(1+.1*(n-5));
        Ks2 = k^2+m^2;
        mu = sqrt((-2*lam2+Ks2)./(Ks2+2*lam2));
        nu1 = k*Um + k*UT*mu;                   % mode 1
        
        
        psiM = psiM +psiM0.*exp(i*(k*x*1000-nu1*t));
        psiT =psiT +mu.*psiM0.*exp(i*(k*x*1000-nu1*t));
    end 
    
    t = + dt*j;
    figure(1)
    axis square
    set(gca,'NextPlot','replacechildren')
    hold off
    % psi1 and psi3 are plotted 
    psi1 = real(psiM+psiT);
    psi3 = real(psiM-psiT);
    
    psi1 = psi1.*sin(m*y*1.e3);
    psi3 = psi3.*sin(m*y*1.e3);
    subplot(2,1,1)
    pcolor(x,y,psi1), title('250 mb streamfunction  (m^2/s)')
    caxis([-1.e7 1.e7])
    xlabel('x  (km)'), ylabel('y  (km)');
    shading interp
    hold on
    subplot(2,1,2)
    pcolor(x,y,psi3), title('750 mb streamfunction  (m^2/s)')
    caxis([-1.e7 1.e7])
    xlabel('x  (km)'), ylabel('y  (km)')
    shading interp
    str1 = ['time = ' num2str(t/3600) ' hours'];
    text(-2000, 2000, str1,'Fontsize', 12)
    hold off
    
    H = gcf;
    M(:,j) = getframe(H);
end 
movie(H,M,2,4)
hold off

disp('to replay animation twice type:   movie(gcf,M,2,4)' )
