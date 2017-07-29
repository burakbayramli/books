% MATLAB file  trajectory_1.m
% Trajectory problem for Chapter 3 requires function zprim1.m.
% This version animates streamlines and 1/2 day segments of trajectories.
% Script to compute trajectory in Cartesian coordinates for barotropic
% motion with zonally propagating sinusoidal wave streamfunction.
% Velocities are input in m/s.
% Objective is to study trajectories for waves with closed streamlines
% First define the grid points on which fields are computed:
% (Distances are in units of  km).
clear all
close all
disp('use different figure number for each run of the script to aid comparisons')
nf = input('specify a figure number (1,2, 3, etc.)  ')
xx=linspace(-3000,3000,30);         % 30 gridpoints in x   
yy=linspace( -1000,1000,20);        % 20 gridpoints in y
[x,y]=meshgrid(xx*1000,yy*1000);    % Sets matrix for grid system in x and y in meters
disp('Trajectories are plotted for particles initially at y= -750,0,+750 km')
x0 = 1000*input(' Specify initial x position in range -3000 <x< +3000 (try  0) ')
x00 = x0;
x1 = x0; x2 = x0;
y0 = -7.5e5; y1 = 0.0; y2 = 7.5e5;
y00=y0; y10=y1; y20=y2;
ub = 0;                             % zero mean zonal wind
up = input('give a perturbation horizontal wind in m/s (try  10) ')
cp = input('give a wave phase speed in m/s (try 5 or 10 or 15)   ')
cor=1.e-4;
k=2*pi/3.e6;
m=pi/2.e6;
time = 10*24*3600;                  % 10 day trajectories
JM = 20 ;                           % number of movie frames to prepare
% phi is contoured with contour lines labelled
figure(nf)
set(gca,'NextPlot','replacechildren')
for j = 1:JM;
    tt = 0.5*(j-1)*24*3600 ;         % time for plotting in seconds
    t1 = 0.5*j*24*3600;
    phi = -cor*ub*y +cor*up/k*sin(k*(x-cp*tt)).*cos(m*y);
    top = max(max(phi))/9.8;
    v1= (0:top/4:top);
    contour(x/1000,y/1000,phi/9.8,v1,'k-');
    hold on;
    contour(x/1000,y/1000,phi/9.8, [0 0],'r-');
    v2=(-top:top/4:0);
    contour(x/1000,y/1000,phi/9.8,v2,'k--');
    options = odeset('RelTol', 1.e-5);
    
    % solving for trajectories
    [t,s1] = ode45('zprim1',[tt t1],[ x0 y0 x1  y1 x2 y2],options,ub,up,cp ,k,m);
    % plotting trajectories  with markers
    s=s1/1000;   %convert position to km
    plot(s(:,1),s(:,2),'b','LineWidth',2);
    plot(s(:,3),s(:,4),'r','LineWidth',2);
    plot(s(:,5),s(:,6),'c','LineWidth',2);
    d=size(s);
    plot(s(d(1),1),s(d(1),2),'b*');
    plot(s(d(1),3),s(d(1),4),'r*');
    plot(s(d(1),5),s(d(1),6),'c*');
    
    x0= s1(d(1),1); x1 = s1(d(1),3); x2 = s1(d(1),5);
    y0= s1(d(1),2); y1 = s1(d(1),4); y2 = s1(d(1),6);
    axis([-3000 3000  -1000 1000]);
    xlabel('x');
    ylabel('y');
    title('Trajectory for moving wave')
    M(:,j) = getframe;
    [t,s1] = ode45('zprim1',[0 time],[ x00 y00 x00  y10 x00 +y20],options,ub,up,cp ,k,m);
    % plotting trajectories  with markers
    s=s1/1000;
  
    plot(s(:,1),s(:,2),'b+',s(:,3),s(:,4),'r+',s(:,5),s(:,6),'c+');
  hold off
end








