clear;close all;clc;

% D2Q9 solver
% This is almost a direct translation of the code found in the Mohamad
% textbook.

addpath basic
addpath post

% Numerical input parameters.
nodes = [100, 100]; % x nodes, y nodes.
dh = 1; % dh = dx = dy.
timesteps = 400;
dt = 1; % timestep.

% Physical input parameters.
u0 = 0.1;
rho0 = 5;
% Discrete parameters.
alpha = 0.01;
% Non-dimensional parameters.
Re = u0*nodes(1)/alpha;
disp(['Reynolds number: ' num2str(Re)]);

% Lattice link constants.
w = zeros(9,1);
w(1) = 4/9;
w(2:5) = 1/9;
w(6:9) = 1/36;
c = zeros(9,2);
c(1,:) = [0, 0];
c(2,:) = [1, 0];
c(3,:) = [0, 1];
c(4,:) = [-1, 0];
c(5,:) = [0, -1];
c(6,:) = [1, 1];
c(7,:) = [-1, 1];
c(8,:) = [-1, -1];
c(9,:) = [1, -1];

% Derived inputs.
omega = 1 / ( 3*alpha + 0.5 );

% Initialize.
rho = rho0*ones(nodes(2),nodes(1));
u = zeros(nodes(2),nodes(1));
v = zeros(nodes(2),nodes(1));
f = zeros(nodes(2),nodes(1),9);
feq = zeros(nodes(2),nodes(1),9);
% BC.
u(end,2:end-1) = u0;

% Main loop.
reconstruction_time = 0;
collision_time = 0;
streaming_time = 0;
bc_time = 0;
for iter = 1:timesteps
    disp(['Running timestep ' num2str(iter)]);
    % Collision.
    tic;
    t1 = u.*u + v.*v;
    for k = 1:9
        t2 = c(k,1)*u + c(k,2)*v;
        feq(:,:,k) = w(k)*rho.*(1 + 3*t2 + 4.5*t2.^2 - 1.5*t1);
        f(:,:,k) = omega*feq(:,:,k)+(1-omega)*f(:,:,k);
    end
    collision_time = collision_time + toc;
    % Streaming.
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
    % BC.
    tic;
    f(:,1,2) = f(:,1,4); % West bounceback.
    f(:,1,6) = f(:,1,8); % West bounceback.
    f(:,1,9) = f(:,1,7); % West bounceback.
    f(:,end,4) = f(:,end,2); % East bounceback.
    f(:,end,8) = f(:,end,6); % East bounceback.
    f(:,end,7) = f(:,end,9); % East bounceback.
    f(1,:,3) = f(1,:,5); % South bounceback.
    f(1,:,6) = f(1,:,8); % South bounceback.
    f(1,:,7) = f(1,:,9); % South bounceback.
    rho_end = f(end,2:end-1,1) + f(end,2:end-1,2) + f(end,2:end-1,4) + ...
        2*( f(end,2:end-1,3) + f(end,2:end-1,7) + f(end,2:end-1,6) );
    f(end,2:end-1,5) = f(end,2:end-1,3); % North boundary (moving lid).
    f(end,2:end-1,9) = f(end,2:end-1,7) + (u0 / 6)*rho_end; % North boundary (moving lid).
    f(end,2:end-1,8) = f(end,2:end-1,6) - (u0 / 6)*rho_end; % North boundary (moving lid).
    bc_time = bc_time + toc;
    % Density and velocity reconstruction.
    tic;
    rho = sum(f,3);
    rho(end,2:end) = f(end,2:end,1) + f(end,2:end,2) + f(end,2:end,4) + ...
        2*( f(end,2:end,3) + f(end,2:end,7) + f(end,2:end,6) );
    u(2:end-1,2:end) = 0;
    v(2:end-1,2:end) = 0;
    for k = 1:9
        u(2:end-1,2:end) = u(2:end-1,2:end) + c(k,1)*f(2:end-1,2:end,k);
        v(2:end-1,2:end) = v(2:end-1,2:end) + c(k,2)*f(2:end-1,2:end,k);
    end
    u(2:end-1,2:end) = u(2:end-1,2:end) ./ rho(2:end-1,2:end);
    v(2:end-1,2:end) = v(2:end-1,2:end) ./ rho(2:end-1,2:end);
    reconstruction_time = reconstruction_time + toc;
end

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

% Streamfunction calculation.
strf = zeros(nodes(2),nodes(1));
for i = 2:nodes(1)
    rho_av = 0.5*( rho(1,i-1) + rho(1,i) );
    strf(1,i) = strf(1,i-1) - 0.5*rho_av*( v(1,i-1) + v(1,i) );
    for j = 2:nodes(2)
        rho_m = 0.5 * ( rho(j,i) + rho(j-1,i) );
        strf(j,i) = strf(j-1,i) + 0.5*rho_m*( u(j-1,i) + u(j,i) );
    end
end

% % Plotting results!
figure;
L = dh*[nodes(1)-1, nodes(2)-1] ; % x , y dimensions of physical domain.
x = linspace(0,L(1),nodes(1))';
y = linspace(0,L(2),nodes(2))';
[X, Y] = meshgrid(x,y);
contour(X, Y, strf);
title('Solution');
xlabel('x');
ylabel('y');


