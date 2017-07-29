% MATLAB file:   grp_vel_3.m
% Shows how wave envelope changes when more wave modes added.
% Uses deep water waves example.
% Solves for multi wave example generalizing. (7.5) in text.
% All modes have the same .
% Student should examine how 1/2 width of envelope changes with number
% of wave modes retained.
clear all
close all
N = input('input number of wavenumbers to sum ');
x = -50:.005:150;
c = 20;                     % phase speed
cg = c/2;                   % group velocity
t = 0;
dt = .04;
k = 0.2;
%wavenumber of dominate mode
dk = .1*k;                   % variation in wavenumber
nu = c*k;
dnu = .5*c*dk;
%following 4 statements displace fields vertically for plotting
h1 = 12*ones(size(x));
h2 = 8*ones(size(x));
h3 = 4*ones(size(x));
h4 = zeros(size(x));
%times for plotting
t1=0; t2=1; t3=2; t4=3;
for m=1:N
    n=m-1;
    h1= h1 + (cos((k+n*dk)*x-(nu+n*dnu)*t1)+cos((k-n*dk)*x-(nu-n*dnu)*t1))/N;
    h2= h2 + (cos((k+n*dk)*x-(nu+n*dnu)*t2)+cos((k-n*dk)*x-(nu-n*dnu)*t2))/N;
    h3= h3 + (cos((k+n*dk)*x-(nu+n*dnu)*t3)+cos((k-n*dk)*x-(nu-n*dnu)*t3))/N;
    h4= h4 + (cos((k+n*dk)*x-(nu+n*dnu)*t4)+cos((k-n*dk)*x-(nu-n*dnu)*t4))/N;
end
plot(x,h1,x,h2,x,h3,x,h4)
xlabel('x (m)'), ylabel('height (m) each curve displaced upward 4 m')
text(50,10,'total Fourier modes =')
text(100,10,num2str(N))
