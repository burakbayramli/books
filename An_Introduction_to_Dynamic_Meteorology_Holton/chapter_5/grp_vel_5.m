% MATLAB file:  grp_vel_5.m 
% Animation to demonstrate phase versus group velocity.
% Uses example of phase and group velocity opposite directions
% as in a zonally propagating Rossby wave.
% Here the asymtotic limit for many waves with Gaussian shape is
% prescribed.
% Matlab script uses the erase mode described on page 4-47 in online user
% guide.
close all
clear all
x = 0:0.01:80;
c = -20;                % phase speed
cg = -c;                % group velocity
t = 0;
dt = .04;
L = 12;                  % e-folding width of group
k = 1;                   % wavenumber of dominate mode
axis square
for j = 1:60
    t =  dt*j;
    h =  exp(-((x-cg*t)/L).^2).*cos(k*(x-c*t));
    hold off
    set(gca,'NextPlot','replacechildren')
    plot(x,h,x,exp(-((x-cg*t)/L).^2))
    xlabel('x (m)'), ylabel('height (m)')
    title('zonally propagating Rossby wave group')
    H= gcf;
    M(:,j) = getframe(H);
end 
movie(H,M)

