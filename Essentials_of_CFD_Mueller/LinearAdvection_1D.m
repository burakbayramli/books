%% Linear 1D Advection equation
% https://raw.githubusercontent.com/wme7/Aero-matlab/master/CFD/LinearAdvection_1D.m
% Routine for solving the following PDE
%
%    du/dt + a du/dx = 0
%
% Using the following scheemes:
%
% 1. One-sided Upwind
% 2. Lax-Friedrichs
% 3. Lax-Wendroff
% 4. Beam-Warming
%
clc; clear; close all; 
%% Parameters
cfl   = 0.8; % CFL = a*dt/dx
tend  = 0.2; % End time
a     = 0.5; % Scalar wave speed 

%% Domain Discretization
dx = 0.01; 
dt = cfl/abs(a)*dx;
x  = 0:dx:1;
t  = 0:dt:tend;

%% Initial Condition
n   = length(x);
u_0 = zeros(1,n);
u_0(1:floor(n/2)) = 1; u_0(floor(n/2)+1:n) = 0;
u_next = zeros(1,n);

%% 1. One-Sided Upwind
u_upwind = u_0;
for k = t
    for j = 2:n-1
    u_next(j) = u_upwind(j) - cfl*(u_upwind(j)-u_upwind(j-1));
    end
    u_upwind = u_next;
    % BC
    u_upwind(1) = u_next(2);
    u_upwind(n) = u_next(n-1);
end
u_next = zeros(1,n);

%% 2. Lax-Friedrichs
u_LF = u_0;
for k = t
    for j = 2:n-1
    u_next(j) = 1/2*(u_LF(j-1)+u_LF(j+1)) - 1/2*cfl*(u_LF(j+1)-u_LF(j-1));
    end
    u_LF = u_next;
    % BC
    u_LF(1) = u_next(2);
    u_LF(n) = u_next(n-1);
end 
u_next = zeros(1,n);

%% 3. Lax-Wendroff
u_LW = u_0;
for k = t
    for j = 2:n-1
    u_next(j) = u_LW(j) - 1/2*cfl*(u_LW(j+1)-u_LW(j-1)) + ...
        1/2*(cfl^2)*(u_LW(j+1)-2*u_LW(j)+u_LW(j-1));
    end
    u_LW = u_next;
    % BC
    u_LW(1) = u_next(2);
    u_LW(n) = u_next(n-1);
end
u_next = zeros(1,n);

%% 4. Beam-Warming
u_BW = u_0;
for k = t
    for j = 3:n
    u_next(j) = u_BW(j) - 1/2*cfl*(3*u_BW(j)-4*u_BW(j-1)+u_BW(j-2)) + ...
        1/2*(cfl^2)*(u_BW(j)-2*u_BW(j-1)+u_BW(j-2));
    end
    u_BW = u_next;
    % BC
    u_BW(1) = u_next(3);
    u_BW(2) = u_next(3);
end

%% Exact Solution
x_exact = 0.5 + a*tend;
u_exact = zeros(1,n);
for j = 1:n
    if x(j) <= x_exact
    u_exact(j) = 1;
    else
    u_exact(j) = 0;
    end
end

%% Make a comparative Plot
offset = 0.05;
subplot(221); 
hold on
plot(x,u_upwind,'.'); plot(x,u_exact,'k'); xlabel('X-Coordinate [-]'); ylabel('U-state [-]'); ylim([-0.5,1.5]); title 'One-Sided Upwind'; 
hold off
subplot(222); 
hold on
plot(x,u_LF,'.'); plot(x,u_exact,'k'); xlabel('X-Coordinate [-]'); ylabel('U-state [-]'); ylim([-0.5,1.5]); title 'Lax-Friedrichs';
hold off
subplot(223);
hold on
plot(x,u_LW,'.'); plot(x,u_exact,'k'); xlabel('X-Coordinate [-]'); ylabel('U-state [-]'); ylim([-0.5,1.5]); title 'Lax-wendroff';
hold off
subplot(224); 
hold on
plot(x,u_BW,'.'); plot(x,u_exact,'k'); xlabel('X-Coordinate [-]'); ylabel('U-state [-]'); ylim([-0.5,1.5]); title 'Beam-Warming';
hold off
