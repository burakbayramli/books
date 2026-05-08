% Cavity flow with non-uniform grid.
% D2Q9 solver of a lid-driven cavity, with a coarse-to-fine vertical grid 
% refinement interface in the middle.
% Author: Robert Lee
% Email: rlee32@gatech.edu

clear;close all;clc;
addpath bc
addpath refine
addpath basic

% Important notes for the interface.
% 1. Collides never involve the (newly generated) fine interface cells.
% 2. Streams on the fine grid always involve the (newly generated) fine
% interface cells.

% Algorithm steps:
% Initialize meso (f)
% Apply meso BCs
% Determine macro variables and apply macro BCs (coarse and fine)
% Loop:
%   Collide coarse and fine.
%   Apply meso BCs
%   Explode coarse to interface
%   Stream coarse and fine+interface
%   Apply meso BC?
%   Determine macro variables and apply macro BCs? (fine)
%   Collide fine (no interface)
%   Apply fine meso BC?
%   Stream fine+interface
%   Apply fine meso BC?
%   Coalesce interface to coarse
%   Apply coarse meso BC?
%   Determine macro variables and apply macro BCs (coarse and fine)

% The cavity is rotated so that the lid is on the west side, instead of the
% north.
% West: moving wall
% North: wall
% South: wall
% East: wall

% We define a two grids; one fine and one coarse.
% The fine and coarse differ by only one level of refinement (4x cells).
% The fine and coarse grids have a one-layer overlapping interface.
% The fine and coarse grids are both square.

% Physical parameters.
u_p = 0.4;
rho_p = 1;
L_p = 0.1; % Height of each channel.
nu_p = 1.568e-5; % kinematic viscosity, m^2/s.
% Grid parameters.
cells_c = 100; % MUST BE EVEN! coarse vertical cells. the number of horizontal cells is half.
dt_c = 0.001; % coarse timestep.
timesteps = 10000;

% Derived nondimensional parameters.
Re = u_p*L_p/nu_p;
disp(['Reynolds number: ' num2str(Re)]);
% Derived numerical parameters.
cells_f = 2*cells_c;
dh_c = 1 / cells_c; % coarse spacing.
dh_f = dh_c / 2; % coarse spacing.
dt_f = dt_c / 2;
nu_lb_c = dt_c / dh_c^2 / Re; % coarse viscosity.
nu_lb_f = dt_f / dh_f^2 / Re; % fine viscosity.
tau_c = 3*nu_lb_c + 0.5; % coarse relaxation time.
tau_f = 3*nu_lb_f + 0.5; % fine relaxation time.
omega_c = 1 / ( 3*tau_c + 0.5 );
omega_f = 1 / ( 3*tau_f + 0.5 );
u_lb_c = dt_c / dh_c;
u_lb_f = dt_f / dh_f;
disp(['Relaxation time ratio: ' num2str(tau_f/tau_c)]);
m = dh_c / dh_f; % spacing ratio;

% Initialize.
f_c = ones(cells_c,cells_c/2,9);
f_f = ones(cells_f,cells_f/2,9);
% Apply meso BCs.
f_c = moving_wall_bc_all(f_c,u_lb_c,'west');
f_c = wall_bc(f_c,'north');
f_c = wall_bc(f_c,'south');
f_f = wall_bc(f_f,'east');
f_f = wall_bc(f_f,'north');
f_f = wall_bc(f_f,'south');
% Determine macro variables and apply macro BCs (coarse and fine)
[u_c, v_c, rho_c] = reconstruct_macro_all(f_c);
u_c(end,:) = 0;
v_c(end,:) = 0;
u_c(1,:) = 0;
v_c(1,:) = 0;
u_c(2:end-1,1) = 0;
v_c(2:end-1,1) = u_lb_c;
[u_f, v_f, rho_f] = reconstruct_macro_all(f_f);
u_f(end,:) = 0;
v_f(end,:) = 0;
u_f(1,:) = 0;
v_f(1,:) = 0;
u_f(:,end) = 0;
v_f(:,end) = 0;

% Main loop.
disp(['Running ' num2str(timesteps) ' timesteps...']);
for iter = 1:timesteps
    
    % Collide on both.
    f_c = collide(f_c,u_c,v_c,rho_c,omega_c);
    f_f = collide(f_f,u_f,v_f,rho_f,omega_f);
    
    % Apply meso BCs.
    f_c = moving_wall_bc_all(f_c,u_lb_c,'west');
    f_c = wall_bc(f_c,'north');
    f_c = wall_bc(f_c,'south');
    f_f = wall_bc(f_f,'east');
    f_f = wall_bc(f_f,'north');
    f_f = wall_bc(f_f,'south');
    
    % Explode coarse cells and map to fine.
    % f_i: fine layer corresponding to eastmost column of the coarse grid. 
    % f_i takes streams FROM the east from the fine grid.
    f_i = zeros(cells_f, 2, 9);
    scale = 1; % 1/m*tau_f/tau_c;
    for k = 1:9
        f_i(:,:,k) = explode_column_cells(scale*f_c(:,:,k));
    end
    
    % Stream coarse and fine+interface
    f_c = stream(f_c);
    f_if = stream([f_i, f_f]);
    f_i = f_if(:,1:2,:);
    f_f = f_if(:,3:end,:);
    
    % Apply meso BCs.
    f_c = moving_wall_bc_all(f_c,u_lb_c,'west');
    f_c = wall_bc(f_c,'north');
    f_c = wall_bc(f_c,'south');
    f_f = wall_bc(f_f,'east');
    f_f = wall_bc(f_f,'north');
    f_f = wall_bc(f_f,'south');

    %   Determine macro variables and apply macro BCs? (fine)
    [u_f, v_f, rho_f] = reconstruct_macro_all(f_f);
    u_f(end,:) = 0;
    v_f(end,:) = 0;
    u_f(1,:) = 0;
    v_f(1,:) = 0;
    u_f(:,end) = 0;
    v_f(:,end) = 0;
    
    %   Collide fine (no interface)
    f_f = collide(f_f,u_f,v_f,rho_f,omega_f);
    
    %   Apply fine meso BC?
    f_f = wall_bc(f_f,'east');
    f_f = wall_bc(f_f,'north');
    f_f = wall_bc(f_f,'south');
    
    %   Stream fine+interface
    f_if = stream([f_i, f_f]);
    f_i = f_if(:,1:2,:);
    f_f = f_if(:,3:end,:);
    
    %   Apply fine meso BC?
    f_f = wall_bc(f_f,'east');
    f_f = wall_bc(f_f,'north');
    f_f = wall_bc(f_f,'south');
    
    %   Coalesce interface to coarse
    for k = [4, 7, 8]
        f_c(:,end,k) = coalesce_column_cells(f_i(:,:,k));
    end
    
    %   Apply coarse meso BC?
    f_c = moving_wall_bc_all(f_c,u_lb_c,'west');
    f_c = wall_bc(f_c,'north');
    f_c = wall_bc(f_c,'south');

    % Determine macro variables and apply macro BCs (coarse and fine)
    [u_c, v_c, rho_c] = reconstruct_macro_all(f_c);
    u_c(end,:) = 0;
    v_c(end,:) = 0;
    u_c(1,:) = 0;
    v_c(1,:) = 0;
    u_c(2:end-1,1) = 0;
    v_c(2:end-1,1) = u_lb_c;
    [u_f, v_f, rho_f] = reconstruct_macro_all(f_f);
    u_f(end,:) = 0;
    v_f(end,:) = 0;
    u_f(1,:) = 0;
    v_f(1,:) = 0;
    u_f(:,end) = 0;
    v_f(:,end) = 0;
    
    % VISUALIZATION
    % Modified from Jonas Latt's cavity code on the Palabos website.
    if (mod(iter,10)==0)
        uu_c_ = sqrt(u_c.^2+v_c.^2);
        uu_c = zeros(cells_f,cells_f/2);
        uu_c(1:2:end,1:2:end) = uu_c_;
        uu_c(1:2:end,2:2:end) = uu_c_;
        uu_c(2:2:end,1:2:end) = uu_c_;
        uu_c(2:2:end,2:2:end) = uu_c_;
        uu_f = sqrt(u_f.^2+v_f.^2);
        
        % This shows cell sizes at the grid interface.
        uu_c(1:4:end,end-1) = 0;
        uu_c(2:4:end,end-1) = 0;
        uu_c(1:4:end,end) = 0;
        uu_c(2:4:end,end) = 0;
        uu_f(1:2:end,1) = 0;
        uu_f(2:2:end,2) = 0;

        uu = [uu_c, uu_f];
        imagesc((uu'/u_lb_c));
        colorbar
        axis equal off; drawnow
    end
end
disp('Done!');




