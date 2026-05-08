% Channel flow with non-uniform grid. 
% D2Q9 solver of a simple channel, with a coarse-to-fine vertical grid 
% interface in the middle of the channel.
% Author: Robert Lee
% Email: rlee32@gatech.edu

clear;close all;clc;

addpath basic
addpath refine

% Currently working! New graphical plotter! Works great! Success!
% Note to self: streamfunction calculator (from Mohamed text) does not work
% properly here.

% This differs from the other channel_refine files:
% -Cell centers are not located right on the boundary, cell edges are.
% -The interface layer is separate from the grids.

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

% West: fixed-velocity inlet
% North: wall
% South: wall 
% East: 0th-order-extrapolation outlet.

% We define a two grids; one fine and one coarse.
% The fine and coarse differ by only one level of refinement (4x cells).
% The fine and coarse grids have a one-layer overlapping interface.
% The fine and coarse grids are both square.

% Physical parameters.
u_p = 0.1;
rho_p = 0.1;
L_p = 1; % Height of each channel.
nu_p = 1.568e-5; % kinematic viscosity, m^2/s.
% Grid parameters.
cells_c = 40; % coarse cells.
dt_c = .001; % coarse timestep.
timesteps = 1000;
 
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
f_c = 5*ones(cells_c,cells_c,9);
f_f = 5*ones(cells_f,cells_f,9);
% Apply meso BCs.
f_f = outlet_bc(f_f,'east');
f_f = wall_bc(f_f,'south');
f_f = wall_bc(f_f,'north');
f_c = inlet_bc(f_c,u_lb_c,'west');
f_c = wall_bc(f_c,'south');
f_c = wall_bc(f_c,'north');
% Determine macro variables and apply macro BCs (coarse and fine)
[u_c, v_c, rho_c] = reconstruct_macro_all(f_c);
u_c(:,1) = u_lb_c;
v_c(:,1) = 0;
[u_f, v_f, rho_f] = reconstruct_macro_all(f_f);

% Main loop.
disp(['Running ' num2str(timesteps) ' timesteps...']);
for iter = 1:timesteps
    
    % Collide on both.
    f_c = collide(f_c,u_c,v_c,rho_c,omega_c);
    f_f = collide(f_f,u_f,v_f,rho_f,omega_f);
    
    % Apply meso BCs.
    f_f = outlet_bc(f_f,'east');
    f_f = wall_bc(f_f,'south');
    f_f = wall_bc(f_f,'north');
    f_c = inlet_bc(f_c,u_lb_c,'west');
    f_c = wall_bc(f_c,'south');
    f_c = wall_bc(f_c,'north');
    
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
    f_f = outlet_bc(f_f,'east');
    f_f = wall_bc(f_f,'south');
    f_f = wall_bc(f_f,'north');
    f_c = inlet_bc(f_c,u_lb_c,'west');
    f_c = wall_bc(f_c,'south');
    f_c = wall_bc(f_c,'north');

    %   Determine macro variables and apply macro BCs? (fine)
    [u_f, v_f, rho_f] = reconstruct_macro_all(f_f);
    
    %   Collide fine (no interface)
    f_f = collide(f_f,u_f,v_f,rho_f,omega_f);
    
    %   Apply fine meso BC?
    f_f = outlet_bc(f_f,'east');
    f_f = wall_bc(f_f,'south');
    f_f = wall_bc(f_f,'north');
    
    %   Stream fine+interface
    f_if = stream([f_i, f_f]);
    f_i = f_if(:,1:2,:);
    f_f = f_if(:,3:end,:);
    
    %   Apply fine meso BC?
    f_f = outlet_bc(f_f,'east');
    f_f = wall_bc(f_f,'south');
    f_f = wall_bc(f_f,'north');
    
    %   Coalesce interface to coarse
    for k = [4, 7, 8]
        f_c(:,end,k) = coalesce_column_cells(f_i(:,:,k));
    end
    
    %   Apply coarse meso BC?
    f_c = inlet_bc(f_c,u_lb_c,'west');
    f_c = wall_bc(f_c,'south');
    f_c = wall_bc(f_c,'north');

    % Determine macro variables and apply macro BCs (coarse and fine)
    [u_c, v_c, rho_c] = reconstruct_macro_all(f_c);
    u_c(:,1) = u_lb_c;
    v_c(:,1) = 0;
    [u_f, v_f, rho_f] = reconstruct_macro_all(f_f);
    
    % VISUALIZATION
    % Modified from Jonas Latt's cavity code on the Palabos website.
    if (mod(iter,10)==0)
        uu_c_ = sqrt(u_c.^2+v_c.^2);
%         uu_c_(1:2:end,end-1) = 0;
%         uu_c_(2:2:end,end) = 0;
        uu_c = zeros(cells_f,cells_f);
        uu_c(1:2:end,1:2:end) = uu_c_;
        uu_c(1:2:end,2:2:end) = uu_c_;
        uu_c(2:2:end,1:2:end) = uu_c_;
        uu_c(2:2:end,2:2:end) = uu_c_;
        uu_f = sqrt(u_f.^2+v_f.^2);
%         uu_f(1:2:end,1) = 0;
%         uu_f(2:2:end,2) = 0;
        uu = [uu_c, uu_f];
        imagesc(uu/u_lb_c);
        colorbar
        axis equal off; drawnow
    end
end
disp('Done!');




