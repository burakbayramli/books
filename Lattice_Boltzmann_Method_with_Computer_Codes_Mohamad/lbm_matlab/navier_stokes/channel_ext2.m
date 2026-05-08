clear;close all;clc;

% D2Q9 solver
% Simple channel.
% West: fixed-velocity inlet
% North: wall
% South: wall 
% East: 2nd-order-extrapolation outlet.

addpath basic
addpath post

% Numerical input parameters.
nodes = [100, 100]; % x nodes, y nodes.
dh = 1; % dh = dx = dy.
timesteps = 100;
dt = 1; % timestep.

% Physical input parameters.
u0 = 0.1;
rho0 = 5;
alpha = 0.01;
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
u = u0*ones(nodes(2),nodes(1));
v = zeros(nodes(2),nodes(1));
f = zeros(nodes(2),nodes(1),9);
feq = zeros(nodes(2),nodes(1),9);
% Wall BCs.
u(1,:) = 0;
v(1,:) = 0;
u(end,:) = 0;
v(end,:) = 0;

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
    % Only horizontal velocity.
    rho_west = ( 1 / ( 1 - u0 ) ) * ...
        ( f(2:end-1,1,1) + f(2:end-1,1,3) + f(2:end-1,1,5) + ...
        2*( f(2:end-1,1,4) + f(2:end-1,1,7) + f(2:end-1,1,8) ) );
    f(2:end-1,1,2) = f(2:end-1,1,4) + 2 / 3 * u0 * rho_west; % West inlet.
    f(2:end-1,1,6) = f(2:end-1,1,8) + u0 / 6 * rho_west; % West inlet.
    f(2:end-1,1,9) = f(2:end-1,1,7) + u0 / 6 * rho_west; % West inlet.
    u_east = -1 + ( f(2:end-1,end,1) + f(2:end-1,end,3) + f(2:end-1,end,5) + ...
        2*( f(2:end-1,end,2) + f(2:end-1,end,6) + f(2:end-1,end,9) ) ) / rho0;
    f(2:end-1,end,4) = 2*f(2:end-1,end-1,4)...
        - f(2:end-1,end-2,4); % East outlet (extrapolation).
    f(2:end-1,end,8) = 2*f(2:end-1,end-1,8)...
        - f(2:end-1,end-2,8); % East outlet (extrapolation).
    f(2:end-1,end,7) = 2*f(2:end-1,end-1,7)...
        - f(2:end-1,end-2,7); % East outlet (extrapolation).
    f(1,:,3) = f(1,:,5); % South wall.
    f(1,:,6) = f(1,:,8); % South wall.
    f(1,:,7) = f(1,:,9); % South wall.
    f(end,:,5) = f(end,:,3); % North wall.
    f(end,:,9) = f(end,:,7); % North wall.
    f(end,:,8) = f(end,:,6); % North wall.
    bc_time = bc_time + toc;
    % Density and velocity reconstruction.
    tic;
    rho = sum(f,3);
    rho(2:end-1,1) = rho_west;
    rho(1,:) = f(1,:,1) + f(1,:,2) + f(1,:,4) + ...
        2 * ( f(1,:,8) + f(1,:,5) + f(1,:,9) );
    rho(end,:) = f(end,:,1) + f(end,:,2) + f(end,:,4) + ...
        2 * ( f(end,:,7) + f(end,:,3) + f(end,:,6) );
%     rho(2:end-1,end) = f(2:end-1,end,1) + f(2:end-1,end,3) + f(2:end-1,end,5) + ...
%         2 * ( f(2:end-1,end,2) + f(2:end-1,end,6) + f(2:end-1,end,9) );
    u(2:end-1,2:end-1) = 0;
    v(2:end-1,2:end-1) = 0;
    for k = 1:9
        u(2:end-1,2:end-1) = u(2:end-1,2:end-1) + c(k,1)*f(2:end-1,2:end-1,k);
        v(2:end-1,2:end-1) = v(2:end-1,2:end-1) + c(k,2)*f(2:end-1,2:end-1,k);
    end
    u(2:end-1,2:end-1) = u(2:end-1,2:end-1) ./ rho(2:end-1,2:end-1);
    v(2:end-1,2:end-1) = v(2:end-1,2:end-1) ./ rho(2:end-1,2:end-1);
    v(2:end-1,end) = 0;
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

% Plotting results!
figure;
L = dh*[nodes(1)-1, nodes(2)-1] ; % x , y dimensions of physical domain.
x = linspace(0,L(1),nodes(1))';
y = linspace(0,L(2),nodes(2))';
[X, Y] = meshgrid(x,y);
contour(X(2:end,2:end), Y(2:end,2:end), strf(2:end,2:end));
title('Solution');
xlabel('x');
ylabel('y');


