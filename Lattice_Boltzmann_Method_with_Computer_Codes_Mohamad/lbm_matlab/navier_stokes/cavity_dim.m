clear;close all;clc;

% D2Q9 solver
% Same as cavity_mohamad, but just with clearer distinction between the physical,
% nondimensional and numerical/discrete parameters, according to Jonas
% Latt's 2008 paper on the topic.

addpath basic
addpath post

% Physical parameters.
L_p = .1; % Cavity dimension. 
U_p = .1; % Cavity lid velocity.
nu_p = 1.568e-5; % Physical kinematic viscosity.
rho0 = 1;
% Nondimensional parameters.
Re = L_p*U_p/nu_p;
disp(['Reynolds number: ' num2str(Re)]);
% Discrete/numerical parameters.
nodes = 100;
dt = 0.001;
timesteps = 200;

% Derived physical parameters.
t_p = L_p / U_p;
disp(['Physical time scale: ' num2str(t_p) ' s']);
% Derived discrete parameters.
dh = 1/(nodes+1);
nu_lb = dt / dh^2 / Re;
tau = 3*nu_lb + 0.5;
disp(['Relaxation time: ' num2str(tau)]);
omega = 1 / tau;
disp(['Relaxation parameter: ' num2str(omega)]);
u_lb = dt / dh;
disp(['Lattice speed: ' num2str(u_lb)])
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

% Initialize.
rho = rho0*ones(nodes,nodes);
u = zeros(nodes,nodes);
v = zeros(nodes,nodes);
f = zeros(nodes,nodes,9);
feq = zeros(nodes,nodes,9);
% BC.
u(end,2:end-1) = u_lb;

% Main loop.
reconstruction_time = 0;
collision_time = 0;
streaming_time = 0;
bc_time = 0;
for iter = 1:timesteps
    disp(['Running timestep ' num2str(iter)]);
    % Collision.
    tic;
    f = collide(f, u, v, rho, omega);
    collision_time = collision_time + toc;
    % Streaming.
    tic;
    f = stream(f);
    streaming_time = streaming_time + toc;
    % BC.
    tic;
    f = wall_bc(f, 'west');
    f = wall_bc(f, 'east');
    f = wall_bc(f, 'south');
    rho_end = f(end,2:end-1,1) + f(end,2:end-1,2) + f(end,2:end-1,4) + ...
        2*( f(end,2:end-1,3) + f(end,2:end-1,7) + f(end,2:end-1,6) );
    f(end,2:end-1,5) = f(end,2:end-1,3); % North boundary (moving lid).
    f(end,2:end-1,9) = f(end,2:end-1,7) + (u_lb / 6)*rho_end; % North boundary (moving lid).
    f(end,2:end-1,8) = f(end,2:end-1,6) - (u_lb / 6)*rho_end; % North boundary (moving lid).
    bc_time = bc_time + toc;
    % Density and velocity reconstruction.
    tic;
    [u,v,rho] = reconstruct_macro(f,u,v);
%     rho = sum(f,3);
%     u(2:end-1,2:end-1) = 0;
%     v(2:end-1,2:end-1) = 0;
%     for k = 1:9
%         u(2:end-1,2:end-1) = u(2:end-1,2:end-1) + c(k,1)*f(2:end-1,2:end-1,k);
%         v(2:end-1,2:end-1) = v(2:end-1,2:end-1) + c(k,2)*f(2:end-1,2:end-1,k);
%     end
%     u(2:end-1,2:end-1) = u(2:end-1,2:end-1) ./ rho(2:end-1,2:end-1);
%     v(2:end-1,2:end-1) = v(2:end-1,2:end-1) ./ rho(2:end-1,2:end-1);
    reconstruction_time = reconstruction_time + toc;
    % VISUALIZATION
    % Modified from Jonas Latt's cavity code on the Palabos website.
    if (mod(iter,10)==0)
        uu = sqrt(u.^2+v.^2) / u_lb;
        imagesc(flipud(uu));
        colorbar
        axis equal off; drawnow
    end
end

% Checks
% should_be_zero = sum(v(:,1))+sum(v(:,end))+sum(v(1,:))+sum(v(end,:))
% should_be_zero = sum(u(:,1))+sum(u(:,end))+sum(u(1,:))

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
strf = streamfunction(u,v,rho);

% % Plotting results!
figure;
x = linspace(0,L_p,nodes)';
y = linspace(0,L_p,nodes)';
[X, Y] = meshgrid(x,y);
contour(X, Y, strf);
title('Solution');
xlabel('x');
ylabel('y');


