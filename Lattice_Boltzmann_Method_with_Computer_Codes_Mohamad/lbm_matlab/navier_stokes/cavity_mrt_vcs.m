% First version working (only steady, non-advected body force contribution)
%   Steady, non-advected body force distribution allows more stability.
%   Tested at Re=1e5.
% Advecting the body force term can give better results.
% including implicit body force disbution term would be more demanding,
%   calling for an implicit procedure.

% A Lattice Boltzmann Multiple Relaxation Time D2Q9 solver,
% with Viscosity Counteraction (Steady approximation), 
% on a lid-driven cavity.
% For the viscosity counteraction scheme, the steady approximation is used
% (the implicit body force is neglected).
% This features a non-lattice-aligned wall! 
% Cell centers (nodes) are placed on the boundaries. 
% Author: Robert Lee
% Email: rlee32@gatech.edu

clear;close all;clc;

addpath basic
addpath bc
addpath vc

% Algorithm steps:
% Initialize meso (f)
% Apply meso BCs
% Determine macro variables and apply macro BCs
% Loop:
%   Collide
%   Apply meso BCs
%   Stream
%   Apply meso BCs?
%   Determine macro variables and apply macro BCs

% Physical parameters.
L_p = 2.5; %1.1; % Cavity dimension. 
U_p = 50; %1.1; % Cavity lid velocity.
nu_p = 1.2e-3; % 1.586e-5; % Physical kinematic viscosity.
rho0 = 1;
% Discrete/numerical parameters.
nu_c_f = 3; % multiplier of nu_lb (discrete viscosity); nu_lb*nu_c_f = nu_c 
              % stabilizing parameter; viscosity "buffer".
nodes = 100;
dt = .004;
timesteps = 10000;

% Derived nondimensional parameters.
Re = L_p * U_p / nu_p;
disp(['Reynolds number: ' num2str(Re)]);
% Derived physical parameters.
t_p = L_p / U_p;
disp(['Physical time scale: ' num2str(t_p) ' s']);
% Derived discrete parameters.
dh = 1/(nodes-1);
nu_lb = dt / dh^2 / Re;
nu_c = nu_lb*nu_c_f;
disp(['Lattice viscosity: ' num2str(nu_lb)]);
tau = 3*(nu_lb+nu_c) + 0.5;
disp(['Relaxation time: ' num2str(tau)]);
omega = 1 / tau;
disp(['Relaxation parameter: ' num2str(omega)]);
u_lb = dt / dh;
disp(['Lattice speed: ' num2str(u_lb)])

% Initialize.
f = ones(nodes,nodes,9);
g = zeros(nodes,nodes,9); % VC body force distribution.
% Apply meso BCs.
f = moving_wall_bc(f,'north',u_lb);
f = wall_bc(f,'south');
f = wall_bc(f,'east');
f = wall_bc(f,'west');
% Determine macro variables and apply macro BCs
[u,v,rho] = reconstruct_macro_all(f);
u(end,2:end-1) = u_lb;
v(end,2:end-1) = 0;
u(1,:) = 0;
v(1,:) = 0;
u(:,1) = 0;
v(:,1) = 0;
u(:,end) = 0;
v(:,end) = 0;

% Main loop.
disp(['Running ' num2str(timesteps) ' timesteps...']);
for iter = 1:timesteps
    if (mod(iter,timesteps/10)==0)
        disp(['Ran ' num2str(iter) ' iterations']);
    end
    
    % Collision.
    f = collide_mrt_vcs(f, u, v, rho, omega,nu_c,dh,dt);
    
    % Apply meso BCs.
    f = moving_wall_bc(f,'north',u_lb);
    f = wall_bc(f,'south');
    f = wall_bc(f,'east');
    f = wall_bc(f,'west');

    % Streaming.
    f = stream(f);
    
    % Apply meso BCs.
    f = moving_wall_bc(f,'north',u_lb);
    f = wall_bc(f,'south');
    f = wall_bc(f,'east');
    f = wall_bc(f,'west');
    
    % Determine macro variables and apply macro BCs
    [u,v,rho] = reconstruct_macro_all(f);
    u(end,2:end-1) = u_lb;
    v(end,2:end-1) = 0;
    u(1,:) = 0;
    v(1,:) = 0;
    u(:,1) = 0;
    v(:,1) = 0;
    u(:,end) = 0;
    v(:,end) = 0;
    
    % VISUALIZATION
    % Modified from Jonas Latt's cavity code on the Palabos website.
    if (mod(iter,10)==0)
        uu = sqrt(u.^2+v.^2) / u_lb;
        imagesc(flipud(uu));
        colorbar
        axis equal off; drawnow
    end
end
disp('Done!');



