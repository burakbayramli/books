% MATLAB script:  lorenz_model.m 
% Problem for Chapter 13 revised 2/3/03
% Script to integrate "chaotic" Lorenz 3-component system.
% Time differencing by 3rd order Adams-Bashforth.
clear all
close all
disp('initial Y and Z are zero. Specify initial 0 < X < 10 when asked ')
X0 = input('give an initial X value  ');
Y0 = 0;
Z0 = 0;
% define constants of the system
r = 28;
sig = 10;
b = 8/3;
dt = .02;
dt12 = dt/12;
time = input('integration time units (at least 100) ');
ind = 1;
V = [X0 Y0 Z0];                 % vector of initial conditions
vprimn = zeros(1,3);
% vector vprimn has components of dV/dt at t+dt
vprimn(1) = -sig*V(1) + sig*V(2);
vprimn(2) = -V(1)*V(3) + r*V(1)-V(2);
vprimn(3) = V(1)*V(2) -b*V(3);
vprim1 = vprimn;                % dV/dt at t  
vprim2 = vprim1;                % dV/dt  at t -dt
figure(1)
axis([-30 30 -30 30 0 50])
xlabel('X'), ylabel('Y'), zlabel('Z')
title('trajectory in X Y Z space')
hold on
n = 0;                            % counter
for t = 0:dt:time
    
    Vn = V +dt12*(23*vprimn -16*vprim1 +5*vprim2); 
    V = Vn;
    vprim2 = vprim1;
    vprim1 = vprimn;
    vprimn(1) = -sig*V(1) + sig*V(2);
    vprimn(2) = -V(1)*V(3) + r*V(1)-V(2);
    vprimn(3) = V(1)*V(2) -b*V(3);
    
    
    p = plot3(V(1),V(2),V(3),':','EraseMode','none', 'MarkerSize',6);
   
    
    if mod(t,.2) == 0
        n = n+1;
        Xhist(n) = V(1);
        thist(n) = t;
    end
    drawnow
    t = t +dt;
end
grid
figure(2)
plot(thist(1:n),Xhist(1:n))
xlabel('time'); ylabel('X')
title('time series of X component')
%


