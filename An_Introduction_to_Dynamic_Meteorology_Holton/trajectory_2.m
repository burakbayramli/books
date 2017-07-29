% MATLAB file  trajectory_2.m
% Trajectory problem for Chapter 3: requires function zprim2.m
% Script to compute trajectory in Cartesian coordinates for barotropic
% motion with zonally propagating sinusoidal wave streamfunction.
% this version has zonal mean jet centered in channel.
% Trajectories run for 4 days.
% velocities are input in m/s.  
%
clear all
close all
% first define the grid points on which fields are computed:
% (distances are in units of   km).
xx=linspace(-3000,3000,60);         % 60 gridpoints in x   
yy=linspace( -1000,1000,20);        % 20 gridpoints in y
[x,y]=meshgrid(xx*1000,yy*1000);    % Sets matrix for grid system in x and y
%   *********Define the vectors to be plotted*********
disp('Trajectories are plotted for particles initially at y= -750 km,0,+750 km')
x0 = 1000*input(' Specify initial x position in range -3000<x<+3000 (km)  ')
ub = input('give a mean zonal wind in m/s ' )
up = input('give a perturbation horizontal wind in m/s  ')
cp = input('give a wave phase speed in m/s   ')
k=2*pi/3.e6 ;    % zonal wavenumber in km
m=pi/2.e6 ;                         % meridional wavenumber in km
cor=1.e-4 ;                         % Coriolis parameter
phi = (5500*9.8-cor*ub*sin(m*y)/m +cor*up/(k)*sin(k*x).*cos(m*y)); %geopotential height
time = 4*24*3600;                   % run time in seconds
options = odeset('RelTol', 1.e-5);
figure(1)
% phi is contoured with contour lines labelled
subplot(2,1,1)
[cs,h]=contour(x/1000,y/1000,phi/9.8,'k');
title('initial height field and 4 day trajectories')
clabel(cs,h)
ylabel('y'), xlabel('x')
hold on

% solving for trajectories
[t,s] = ode45('zprim2',[0 time],[ x0 -7.5e5 x0  0.0 x0 +7.5e5],options,ub,up,cp ,k,m);
% plotting trajectories  
s=s/1000;
plot(s(:,1),s(:,2),'b','LineWidth',2)
plot(s(:,3),s(:,4),'r','LineWidth',2)
plot(s(:,5),s(:,6),'c','LineWidth',2)
subplot(2,1,2)
% geopotential field at final time
phi = (5500*9.8-cor*ub*sin(m*y)/m +cor*up/(k)*sin(k*(x-cp*time)).*cos(m*y)); 
[cs,h]=contour(x/1000,y/1000,phi/9.8,'k');
clabel(cs,h)
hold on
% plotting trajectories with markers

plot(s(:,1),s(:,2),'b','LineWidth',1);
plot(s(:,3),s(:,4),'r','LineWidth',1);
plot(s(:,5),s(:,6),'c','LineWidth',1);
[t,s] = ode45('zprim2',[0 .5 1 1.5 2 2.5 3 3.5 4 ]*24*3600,...
    [ x0 -7.5e5 x0  0.0 x0 +7.5e5],options,ub,up,cp ,k,m);
s=s/1000;                       % put resulting coordinates in km

plot(s(4,1),s(4,2),'b*')
plot(s(3,1),s(3,2),'b*')
plot(s(2,1),s(2,2),'b*')
plot(s(1,1),s(1,2),'b*')
plot(s(5,1),s(5,2),'b*')
plot(s(6,1),s(6,2),'b*')
plot(s(7,1),s(7,2),'b*')
plot(s(8,1),s(8,2),'b*')
plot(s(9,1),s(9,2),'bsquare')
plot(s(4,3),s(4,4),'r*')
plot(s(3,3),s(3,4),'r*')
plot(s(2,3),s(2,4),'r*')
plot(s(1,3),s(1,4),'r*')
plot(s(5,3),s(5,4),'r*')
plot(s(6,3),s(6,4),'r*')
plot(s(7,3),s(7,4),'r*')
plot(s(8,3),s(8,4),'r*')
plot(s(9,3),s(9,4),'r')
plot(s(9,3),s(9,4),'rsquare')
plot(s(4,5),s(4,6),'c*')
plot(s(3,5),s(3,6),'c*')
plot(s(2,5),s(2,6),'c*')
plot(s(1,5),s(1,6),'c*')
plot(s(5,5),s(5,6),'c*')
plot(s(6,5),s(6,6),'c*')
plot(s(7,5),s(7,6),'c*')
plot(s(8,5),s(8,6),'c*')
plot(s(9,5),s(9,6),'csquare')

axis([-3000 3000 -1000 1000])
xlabel('x (km)'), ylabel('y (km)');
title('Final height field and 4 day trajectories with 1/2 day marks')






