% MATLAB file:   grp_vel_4.m
% Animation to demonstrate phase versus group velocity.
% Uses deep water waves (ship wake) example.
% Here the asymtotic limit for many waves with Gaussian shape is
% prescribed.
% Matlab script uses the erase mode described on page 4-47 in online user
% guide.
close all
clear all
x = 0:0.01:80;
h = zeros(size(x));
c = 40;                 % phase speed
cg = +c/2;              % group velocity
t = 0;
dt = .04;
L = 12;                 % e-folding width of group
k = 1;                  % wavenumber of dominate mode
axis square
for j = 1:60
    t = dt*j;
    h = exp(-((x-cg*t)/L).^2).*cos(k*(x-c*t));
    hold off
    set(gca,'NextPlot','replacechildren')
    plot(x,h,x,exp(-((x-cg*t)/L).^2))
    xlabel('x (m)'); ylabel('height (m)')
    title('zonally propagating deep water wave group')
    H= gcf;
    M(:,j) = getframe(H);
end 
movie(H,M,2)

