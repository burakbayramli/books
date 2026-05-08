clear;close all;clc;

% With non-equal weights, things get more complicated in the main solver
% loop, compared to D1Q2 and D2Q4, which both have lattice links of equal
% weight.

% As you may have noticed, the addition of advection only changes the
% collision step, and not the streaming step nor the BCs (but we did change 
% the type of BC from the diffusion-only version).

% Input parameters.
nodes = [100, 100]; % x nodes, y nodes.
u = 0.1;
v = 0.4;
dh = 1; % dh = dx = dy.
dt = 1; % timestep.
timesteps = 400;
alpha = 1.0; % Physical constant.
twall = 1.0; % Left-hand wall temperature.

% Constants.
w = zeros(9,1);
w(1) = 4/9;
w(2:5) = 1/9;
w(6:9) = 1/36;

% Derived inputs.
L = dh*[nodes(1)-1, nodes(2)-1] ; % x , y dimensions of physical domain.
ck = dh/dt;
csq = ck.^2;
omega = 1 / (3*alpha/(dt*csq)+0.5);

% Initialize.
x = linspace(0,L(1),nodes(1))';
y = linspace(0,L(2),nodes(2))';
f = zeros(nodes(2),nodes(1),9);
feq = zeros(nodes(2),nodes(1),9);
% BC initialize.
for k = 1:9
    f(:,1,k) = w(k)*twall; % left wall.
end

% Main loop.
reconstruction_time = 0;
collision_time = 0;
streaming_time = 0;
bc_time = 0;
for iter = 1:timesteps
    % Reconstruct T.
    tic;
    rho = f(:,:,1);
    for k = 2:9
        rho = rho + f(:,:,k);
    end
    reconstruction_time = reconstruction_time + toc;
    % Collision
    tic;
    feq(:,:,1) = w(1)*rho;
    feq(:,:,2) = w(2)*rho*(1 + 3*u/ck);
    feq(:,:,3) = w(3)*rho*(1 + 3*v/ck);
    feq(:,:,4) = w(4)*rho*(1 - 3*u/ck);
    feq(:,:,5) = w(5)*rho*(1 - 3*v/ck);
    feq(:,:,6) = w(6)*rho*(1 + 3*(u+v)/ck);
    feq(:,:,7) = w(7)*rho*(1 + 3*(-u+v)/ck);
    feq(:,:,8) = w(8)*rho*(1 - 3*(u+v)/ck);
    feq(:,:,9) = w(9)*rho*(1 + 3*(u-v)/ck);
    for k = 1:9
        f(:,:,k) = omega*feq(:,:,k) + (1-omega)*f(:,:,k);
    end
    collision_time = collision_time + toc;
    % Streaming
    tic;
    f(:,2:end,2) = f(:,1:end-1,2); % East vector.
    f(2:end,:,3) = f(1:end-1,:,3); % North vector.
    f(:,1:end-1,4) = f(:,2:end,4); % West vector.
    f(1:end-1,:,5) = f(2:end,:,5); % South vector.
    f(2:end,2:end,6) = f(1:end-1,1:end-1,6); % Northeast vector.
    f(2:end,1:end-1,7) = f(1:end-1,2:end,7); % Northwest vector.
    f(1:end-1,1:end-1,8) = f(2:end,2:end,8); % Southwest vector.
    f(1:end-1,2:end,9) = f(2:end,1:end-1,9); % Southeast vector.
    streaming_time = streaming_time + toc;
    % Boundary conditions
    tic;
    f(:,1,2) = w(2)*twall + w(4)*twall - f(:,1,4); % Left boundary, T = 1.0.
    f(:,1,6) = w(6)*twall + w(8)*twall - f(:,1,8); % Left boundary, T = 1.0.
    f(:,1,9) = w(9)*twall + w(7)*twall - f(:,1,7); % Left boundary, T = 1.0.
    f(:,end,4) = -f(:,end,2); % Right boundary, T = 0.
    f(:,end,7) = -f(:,end,9); % Right boundary, T = 0.
    f(:,end,8) = -f(:,end,6); % Right boundary, T = 0.
    f(:,end,3) = -f(:,end,5); % Right boundary, T = 0.
    f(:,end,1) = 0; % Right boundary, T = 0.
    f(end,:,5) = -f(end,:,3); % Top boundary, T = 0.
    f(end,:,8) = -f(end,:,6); % Top boundary, T = 0.
    f(end,:,9) = -f(end,:,7); % Top boundary, T = 0.
    f(end,:,2) = -f(end,:,4); % Top boundary, T = 0.
    f(end,:,1) = 0; % Top boundary, T = 0.
%     for k = 2:9
%         f(1,:,k) = f(2,:,k); % Bottom boundary, adiabatic.
%     end
    f(1,:,3) = -f(1,:,5); % Bottom boundary, T = 0.
    f(1,:,7) = -f(1,:,9); % Bottom boundary, T = 0.
    f(1,:,6) = -f(1,:,8); % Bottom boundary, T = 0.
    f(1,:,2) = -f(1,:,4); % Bottom boundary, T = 0.
    f(1,:,1) = 0; % Bottom boundary, T = 0.
    bc_time = bc_time + toc;
end
tic;
rho = f(:,:,1);
for k = 2:9
    rho = rho + f(:,:,k);
end
reconstruction_time = reconstruction_time + toc;

% Timing outputs.
total_time = reconstruction_time + collision_time + streaming_time + bc_time;
disp(['Solution reconstruction time (s): ' num2str(reconstruction_time)]);
disp(['Collision time (s): ' num2str(collision_time)]);
disp(['Streaming time (s): ' num2str(streaming_time)]);
disp(['BC time (s): ' num2str(bc_time)]);
disp(['Solution reconstruction fraction: ' num2str(reconstruction_time/total_time)]);
disp(['Collision fraction: ' num2str(collision_time/total_time)]);
disp(['Streaming fraction: ' num2str(streaming_time/total_time)]);
disp(['BC fraction: ' num2str(bc_time/total_time)]);

% Plotting results!
[X, Y] = meshgrid(x,y);
contour(X, Y, rho);
title('Solution');
xlabel('x');
ylabel('y');





