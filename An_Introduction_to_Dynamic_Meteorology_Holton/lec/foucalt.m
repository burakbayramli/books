% MATLAB script: foucalt.m     (Demo file)
% Foucalt pendulum observed in rotating and fixed coordinates
% from unpublished paper by Norman Phillips.
% Example is for very long pendulum with period about 4 hours.
% Solution gives path of pendulum in local coordinates with equilibrium
% position at (x,y) = (0,0). The pendulum is initially held
% at rest on the rotating earth at (x,y) = (0,1), then released.
close all
clear all 
zeta0 = 0 +i;           % initial displacement in meters
omega = 7.292e-5;       % angular frequency of earth rotation
theta = input('Enter latitude in degrees  ');
omth = omega*sin(theta*pi/180);
ompend = 6*omega;       % frequency of pendulum
%
t = 0;
dt = .01*2*pi/omega;
figure(1)
    hold off
for j = 1:100
    t = dt*j;
    S = ['time in days =  ' num2str(t*omega/(2*pi))];
    zetar(j) = zeta0*exp(-i*omth*t)*(cos(ompend*t) +...
        i*omth/ompend*sin(ompend*t));
    zetan(j) = zeta0*exp(+i*(omega-omth)*t)*(cos(ompend*t) +...
        i*omth/ompend*sin(ompend*t));
    set(gca, 'NextPlot','replacechildren')
    subplot(1,2,1)
    plot(zetar)
    axis square 
    axis([-1 1 -1 1])
    title('trajectory in rotating x,y space')
    text (0.8,1.5,S);
    xlabel('x'), ylabel('y')
    axis square
    subplot(1,2,2)
    plot(zetan)
    axis square 
    axis([-1 1 -1 1]) 
    title('trajectory in fixed x,y space')
    xlabel('x'), ylabel('y')
  
    %  make movie
    H = gcf;
    M(:,j) = getframe(H);
end
movie(H,M)




