clear;close all;clc;

% Input parameters.
nodes = [100, 100]; % x nodes, y nodes.
u = 0.1;
v = 0.2;
dh = 1; % dh = dx = dy.
dt = 1; % timestep.
timesteps = 200;
alpha = 1; % Physical constant.
twall = 1.0; % Left-hand wall temperature.

% Derived inputs.
L = dh*[nodes(1)-1, nodes(2)-1] ; % x , y dimensions of physical domain.
ck = dh/dt;
csq = ck.^2;
omega = 1 / (2*alpha/(dt*csq)+0.5);

% Initialize.
rho = zeros(nodes(2),nodes(1));
f1 = zeros(nodes(2),nodes(1));
f2 = zeros(nodes(2),nodes(1));
f3 = zeros(nodes(2),nodes(1));
f4 = zeros(nodes(2),nodes(1));

% Main loop.
for iter = 1:timesteps
    % Collision
    feq1 = 0.25*rho*(1 + 2*u/ck);
    feq2 = 0.25*rho*(1 - 2*u/ck);
    feq3 = 0.25*rho*(1 + 2*v/ck);
    feq4 = 0.25*rho*(1 - 2*v/ck);
    f1 = omega*feq1 + (1-omega)*f1;
    f2 = omega*feq2 + (1-omega)*f2;
    f3 = omega*feq3 + (1-omega)*f3;
    f4 = omega*feq4 + (1-omega)*f4;
    % Streaming
    f1(:,2:end) = f1(:,1:end-1);
    f2(:,1:end-1) = f2(:,2:end);
    f3(2:end,:) = f3(1:end-1,:);
    f4(1:end-1,:) = f4(2:end,:);
    % Boundary conditions
    f1(2:end,1) = 0.5 - f2(2:end,1); % Left boundary, T = 1.0.
    f3(2:end,1) = 0.5 - f4(2:end,1); % Left boundary, T = 1.0.
    f1(2:end,end) = 0; % Right boundary, T = 0.
    f2(2:end,end) = 0; % Right boundary, T = 0.
    f3(2:end,end) = 0; % Right boundary, T = 0.
    f4(2:end,end) = 0; % Right boundary, T = 0.
%     f1(1,2:end) = 0; % Bottom boundary, T = 0.
%     f2(1,2:end) = 0; % Bottom boundary, T = 0.
%     f3(1,2:end) = 0; % Bottom boundary, T = 0.
%     f4(1,2:end) = 0; % Bottom boundary, T = 0.
    f1(1,2:end) = f1(2,2:end); % Bottom boundary, Adiabatic.
    f2(1,2:end) = f2(2,2:end); % Bottom boundary, Adiabatic.
    f3(1,2:end) = f3(2,2:end); % Bottom boundary, Adiabatic.
    f4(1,2:end) = f4(2,2:end); % Bottom boundary, Adiabatic.
    f1(end,2:end) = 0; % Top boundary, T = 0.
    f2(end,2:end) = 0; % Top boundary, T = 0.
    f3(end,2:end) = 0; % Top boundary, T = 0.
    f4(end,2:end) = 0; % Top boundary, T = 0.
    % Compute solution
    rho = f1+f2+f3+f4;
end

% Plotting results!
x = linspace(0,L(1),nodes(1))';
y = linspace(0,L(2),nodes(2))';
[X, Y] = meshgrid(x,y);
contour(X, Y, rho);
title('Solution');
xlabel('x');
ylabel('y');





