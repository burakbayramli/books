clear; close all; clc;

% Input parameters.
nodes = 100;
u = 0.1;
dt = 1.0;
timesteps = 400;
dx = 1.0;
twall = 1.0; % left-hand wall temperature.

% Constants.
alpha = 0.25;

% Derived inputs.
L = dx*(nodes-1);
ck = dx/dt;
csq = ck.^2;
omega = 1 / ( alpha / (dt*csq) + 0.5 );

% Initial conditions.
f1 = zeros(nodes,1);
f2 = zeros(nodes,1);

% Main loop.
for iter = 1:timesteps
    % Reconstruct solution.
    rho = f1 + f2;
    % Collision.
    feq1 = 0.5*rho*(1 + u / ck); % extra term added to simulate advection.
    feq2 = 0.5*rho*(1 - u / ck); % w1 = w2 = 0.5
    f1 = (1-omega)*f1 + omega*feq1;
    f2 = (1-omega)*f2 + omega*feq2;
    % Streaming.
    f1(2:end-1) = f1(1:end-2);
    f2(1:end-1) = f2(2:end);
    % BC.
    f1(1) = twall - f2(1); % x = 0, T = twall.
    f1(end) = f1(end-1); % x = L, adiabatic.
    f2(end) = f2(end-1); % x = L, adiabatic.
end
rho = f1 + f2;

% Plot results.
x = linspace(0,L,nodes);
plot(x,rho);
title('Solution');
xlabel('x');
ylabel('\rho');
