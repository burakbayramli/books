% MATLAB file:  grp_vel_2.m
% Compares wave fields at 4 times on one plot.
% Uses deep water waves example.
% Solves for 2 wave example of eq. (7.5) in text.

clear all
close all
x = 0:.01:80;
c = 20;                 % phase speed
cg = c/2;               % group velocity
t = 0;
dt = .04;
k = .5;                 % wavenumber of dominate mode
dk = .1*k;              % variation in wavenumber
nu=c*k;
dnu=0.05*nu;

figure(1)
t1 = 1; t2=1.5; t3=2; t4=2.5;
h1 = 12+ cos((k+dk)*x-(nu+dnu)*t1)+cos((k-dk)*x-(nu-dnu)*t1);
h2 = 8+ cos((k+dk)*x-(nu+dnu)*t2)+cos((k-dk)*x-(nu-dnu)*t2);
h3 = 4+cos((k+dk)*x-(nu+dnu)*t3)+cos((k-dk)*x-(nu-dnu)*t3);
h4 = cos((k+dk)*x-(nu+dnu)*t4)+cos((k-dk)*x-(nu-dnu)*t4);
plot(x,h1,x,h2,x,h3,x,h4)
xlabel('x (m)')
ylabel('height (m) each curve displaced upward by 2 m')
