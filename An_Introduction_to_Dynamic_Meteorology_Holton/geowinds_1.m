%geowinds_1.m
% Matlab script to plot geopoential height data and vector winds 
% This version requires the mapping toolbox
% Finite differences used to obtain geostrophic zonal wind,
% Student is to obtain geostrophic meridional wind,
% vorticity, and geostrophic vorticity.
%.Data required:  geop500mb981110.txt, urel500mb981110.txt, and
% vrel500mb981110.txt. Also requires script mapUS.m
% This example uses traditional looping over the grid indices
% to compute finite difference estimates for ug: 
%     ug = -(1/f) d[phi]/dy
%  ug(j,i) = (phi(j+1,i)-phi(j-1,i))/(2*f(j)*dy)
% Note: that all matrices are stored as a(j,i) where j is latitude
% grid and i is longitude grid
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Read in the Data
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close all
urel = load('urel500mb98111000.txt');
vrel = load('vrel500mb98111000.txt');
Z500 = load('geop500mb98111000.txt');

latr = [20 : 1.25 : 60];               %latitude range of phi grid
lonr = [220 : 1.25 : 300];             %longitude range of phi grid

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Compute and plot geostrophic zonal wind using finite difference formula.
% You will copy this model for the meridional component of the wind.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

J = size(latr,2);                % number of grid points in latitude
I = size(lonr,2);                % number of grid points in longitude
ug500=zeros([J I]);                 % set size of ug matrix
ug500(1:J,1:I)=NaN;                 % set all values to NaN
vg500=zeros([J I]);                   %set size of vg matrix
vg500(1:J,1:I)=NaN;                   % set all values to NaN 


dx = 1.11e5*1.25*cos(latr*pi/180); %grid distance in zonal direction
dy = 6.37e6*1.25*pi/180;         % grid distance in meridional direction
cor= 2*7.292e-5*sin(latr*pi/180);  % Coriolis parameter
for i=2:I-1
    for j=2:J-1
        ug500(j,i) = - 9.81*(Z500(j+1,i)-Z500(j-1,i))/(cor(j)*dy*2);
    end 
end
%% prepare grid for color maps
latv=zeros([J I]);                 % This step defines the size of the latv, 
lonv=zeros([J I]);                 % lonv arrays and fills them with zeros
for i=1:I
    for j=1:J
        latv(j,i)=latr(j);
        lonv(j,i)=lonr(i);
    end
end
%Compute the magnitude of horizontal wind
Vobs=sqrt(urel.^2 +vrel.^2);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% First Plot Geopotential Height and wind vectors
%
% Contour limits and intervals for geopotential height
%
% clow=4680; chigh=6000; cint=60;         % 500 mb
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

figure(1)                 
mapUS                             % make background map of US
clow=4680; chigh=6000; cint=60;   % contour line limits for 500 phi
conts = [clow:cint:chigh];        % define contour lines
V = [clow:cint*2:chigh];          % label every 2th line

[c,h]=contorm(latr,lonr,Z500,conts,'linewidth',1.5,'color','k');
clabelm(c,h,V)                   %puts in contours
quiverm(latv,lonv,vrel,urel)
title('Geopotential height and observed vector wind at 500 hPa 98111000')
xlabel('longitude (degrees)'), ylabel('latitude (degrees)')

figure(2);                         
mapUS                               %background map of US
clow=4680; chigh=6000; cint=60;     %contour line limits for 500 phi
conts = [clow:cint:chigh];           %define contour lines
V = [clow:cint*2:chigh];            % label every 4th line
[c,h]=contorm(latr,lonr,Z500,conts,'Linewidth',1.5);
clabelm(c,h,V);                      %puts in contours
pcolorm(latv,lonv,Vobs);
shading interp
colorbar('v');
title('Geopotential height and magnitude observed Wind at 500 hPa 98111000')
xlabel('longitude (degrees)'), ylabel('latitude (degrees)')

figure(3);       %student to add quiver plot of geostrophic wind
mapUS
clow=4680; chigh=6000; cint=60;      %contour line limits for 500 phi
conts = [clow:cint:chigh];           %define contour lines
V = [clow:cint*2:chigh];             % label every 4th line
[c,h]=contorm(latr,lonr,Z500,conts,'linewidth',1.5,'color','k');
clabelm(c,h,V);                      %puts in contours

title('Geopotential height and geostrophic wind at 500 hPa 98111012')
xlabel('longitude (degrees)'), ylabel('latitude (degrees)')
figure(4)  %student to add magnitude of geostrophic wind
mapUS
clow=4680; chigh=6000; cint=60;   %contour line limits for 500 phi
conts = [clow:cint:chigh];        %define contour lines
V = [clow:cint*2:chigh];          % label every 4th line
[c,h]=contorm(latr,lonr,Z500,conts,'linewidth',1,'color','k');
clabelm(c,h,V);                   %puts in contours
title('Geopotential height and  mag V geostrophic at 500 hPa')
xlabel('longitude (degrees)'), ylabel('latitude (degrees)')

figure(5)  %student to add vorticity of observed wind
mapUS
clow=4680; chigh=6000; cint=60;   %contour line limits for 500 phi
conts = [clow:cint:chigh];        %define contour lines
V = [clow:cint*2:chigh];          % label every 4th line
[c,h]=contorm(latr,lonr,Z500,conts,'linewidth',1,'color','k');
clabelm(c,h,V);                   %puts in contours
title('Geopotential height and  vorticity at 500 hPa')
xlabel('longitude (degrees)'), ylabel('latitude (degrees)')

figure(6)               %student to add geostrophic vorticity
mapUS
clow=4680; chigh=6000; cint=60;   %contour line limits for 500 phi
conts = [clow:cint:chigh];        %define contour lines
V = [clow:cint*2:chigh];          % label every 4th line
[c,h]=contorm(latr,lonr,Z500,conts,'linewidth',1,'color','k');
clabelm(c,h,V)                   %puts in contours
title('Geopotential height and  mag V geostrophic at 500 hPa')
xlabel('longitude (degrees)'), ylabel('latitude (degrees)')

figure(7)        %student to add vorticity minus geostrophic vorticity 
mapUS
clow=4680; chigh=6000; cint=60;   %contour line limits for 500 phi
conts = [clow:cint:chigh];        %define contour lines
V = [clow:cint*2:chigh];          % label every 4th line
[c,h]=contorm(latr,lonr,Z500,conts,'linewidth',1,'color','k');
clabelm(c,h,V)                   %puts in contours
title('Geopotential height and  mag V geostrophic at 500 hPa')
xlabel('longitude (degrees)'), ylabel('latitude (degrees)')





