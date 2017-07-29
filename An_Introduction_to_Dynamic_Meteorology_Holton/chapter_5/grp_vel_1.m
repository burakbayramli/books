% MATLAB file:   grp_vel_1.m
% Animation to demonstrate phase versus group velocity.
% Uses deep water wave example.
% Solves for 2 wave example of eq. (7.5) in text.

clear all
close all
x = 0:.01:80;
c = 40;             % phase speed
cg = c/2;           % group velocity
t = 0;
dt = .04;
k = 2;              % wavenumber of dominate mode
dk = .05*k;         % variation in wavenumber
axis square
for j = 1:50
    t = dt*j;
    h = cos(dk*(x-cg*t)).*cos(k*(x-c*t));
    set(gca,'NextPlot','replacechildren')
    plot(x,h)
    xlabel('x (m)'), ylabel('height (m)')
    title('deep water waves')
    H = gcf;
    M(:,j) = getframe(H);
end 
movie(H,M,2)

