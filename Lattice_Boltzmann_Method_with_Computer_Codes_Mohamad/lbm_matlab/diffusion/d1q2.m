clear;close all;clc;

% Input parameters.
nodes = 100; % x nodes, y nodes.
dx = 1; % spacing in x.
dt = 1; % timestep.
timesteps = 200;
alpha = 0.25; % Physical constant.
twall = 1.0; % Left-hand wall temperature.

% Derived inputs.
L = dx*(nodes-1); % x , y dimensions of physical domain.
c = dx/dt;
csq = c.^2;
omega = 1 / (alpha/(dt*csq)+0.5);

% Initialize.
x = linspace(0,L,nodes)';
rho = zeros(nodes,1);
f1 = zeros(nodes,1);
f2 = zeros(nodes,1);

% Main loop.
for iter = 1:timesteps
    % Collision
    rho = f1 + f2;
    feq = 0.5*rho;
    % Since k1 = k2 = 0.5, then feq1 = feq2 = feq.
    f1 = omega*feq + (1-omega)*f1;
    f2 = omega*feq + (1-omega)*f2;
    % Streaming
    f1(2:end-1) = f1(1:end-2);
    f2(1:end-1) = f2(2:end);
    % Boundary conditions
    f1(1) = twall - f2(1); % T = twall, x = 0.
    f1(end) = f1(end-1); % adiabatic, x = L.
    f2(end) = f2(end-1); % adiabatic, x = L.
end

% Plotting results!
plot(x, rho);
title('Solution');
xlabel('x');
ylabel('y');





