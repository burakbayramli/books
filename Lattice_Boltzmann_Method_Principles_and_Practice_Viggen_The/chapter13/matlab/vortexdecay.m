% This code accompanies
%   The Lattice Boltzmann Method: Principles and Practice
%   T. Krüger, H. Kusumaatmaja, A. Kuzmin, O. Shardt, G. Silva, E.M. Viggen
%   ISBN 978-3-319-44649-3 (Electronic) 
%        978-3-319-44647-9 (Print)
%   http://www.springer.com/978-3-319-44647-9
%
% This code is provided under the MIT license. See LICENSE.txt.
%
% Author: Orest Shardt
% 
% Example matlab code for computing a Taylor Green vortex decay using LBM
%

clear all
close all
clc

% simulation parameters
scale = 2;                % set simulation size
NX = 32*scale;            % domain size
NY = NX;
NSTEPS = 200*scale*scale; % number of simulation time steps
NMSG   =  50*scale*scale; %show messages every NMSG time steps
vis = false;              % show visualization; set to false for performance measurements
NVIS = NMSG;              % show visualization every NVIS steps
tau = 1;                  % relaxation time
u_max =  0.04/scale;      % maximum velocity
nu   =  (2*tau-1)/6;      % kinematic shear viscosity
rho0 = 1;                 % rest density
Re = NX*u_max/nu;         % Reynolds number; not used in the simulation itself

% Lattice parameters; note zero direction is last
w  = [1/9 1/9 1/9 1/9 1/36 1/36 1/36 1/36 4/9]; % weights
cx = [1 0 -1  0 1 -1 -1  1 0]; % velocities, x components
cy = [0 1  0 -1 1  1 -1 -1 0]; % velocities, y components

x = (1:NX)-0.5;
y = (1:NY)-0.5;
[X,Y] = meshgrid(x,y);

% Initialize populations
[rho,u,v] = taylorgreen(0,X,Y,NX,NY,nu,rho0,u_max);
f = equilibrium(rho,u,v);

% initialize temporary variable
fprop = zeros(size(f)); % set up propagation

fprintf('Simulating Taylor-Green vortex decay\n');
fprintf('      domain size: %ux%u\n',NX,NY);
fprintf('               nu: %g\n',nu);
fprintf('              tau: %g\n',tau);
fprintf('            u_max: %g\n',u_max);
fprintf('             rho0: %g\n',rho0);
fprintf('        timesteps: %u\n',NSTEPS);
fprintf('       plot every: %u\n',NVIS);
fprintf('    message every: %u\n',NMSG);
fprintf('\n');

E = rho.*(u.*u + v.*v);
E = sum(E(:));
fprintf('%u,%g,%g,%g,%g\n',0,E,0,0,0);

tstart = tic;

% main loop
for t=1:NSTEPS
    % Periodic streaming of whole domain
    % see 'doc circshift' for info
    
    fprop(:,:,1) = circshift(f(:,:,1),[ 0  1]);
    fprop(:,:,2) = circshift(f(:,:,2),[ 1  0]);
    fprop(:,:,3) = circshift(f(:,:,3),[ 0 -1]);
    fprop(:,:,4) = circshift(f(:,:,4),[-1  0]);
    
    fprop(:,:,5) = circshift(f(:,:,5),[ 1  1]);
    fprop(:,:,6) = circshift(f(:,:,6),[ 1 -1]);
    fprop(:,:,7) = circshift(f(:,:,7),[-1 -1]);
    fprop(:,:,8) = circshift(f(:,:,8),[-1  1]);
    
    fprop(:,:,9) = f(:,:,9);
    
    % Compute macroscopic quantities
    % density
    rho = sum(fprop,3);
    
    % momentum components
    jx = sum(fprop(:,:,[1 5 8]),3) - sum(fprop(:,:,[3 6 7]),3);
    jy = sum(fprop(:,:,[2 5 6]),3) - sum(fprop(:,:,[4 7 8]),3);
    
    % velocity components
    u = jx./rho;
    v = jy./rho;
    
    % Compute equilibrium distribution
    feq = equilibrium(rho,u,v);
 
    % Collision step
    f = (1-1/tau)*fprop + (1/tau)*feq;
    
    if mod(t,NMSG) == 0
        % Calculate analytical solution
        [rhoa,uxa,uya] = taylorgreen(t,X,Y,NX,NY,nu,rho0,u_max);
        
        % Kinetic energy
        E = rho.*(u.*u + v.*v);
        E = sum(E(:));
        
        % Sum square errors
        rhoe2 = (rho-rhoa).*(rho-rhoa);
        sumrhoe2 = sum(rhoe2(:));
        
        uxe2 = (u-uxa).*(u-uxa);
        sumuxe2 = sum(uxe2(:));
        
        uye2 = (v-uya).*(v-uya);
        sumuye2 = sum(uye2(:));

        rhoa2 = (rhoa-rho0).*(rhoa-rho0);
        sumrhoa2 = sum(rhoa2(:));
        sumuxa2 = sum(uxa(:).*uxa(:));
        sumuya2 = sum(uya(:).*uya(:));
        
        % L2 norms
        L2rho = sqrt(sumrhoe2/sumrhoa2);
        L2ux = sqrt(sumuxe2/sumuxa2);
        L2uy = sqrt(sumuye2/sumuya2);
        
        fprintf('%u,%g,%g,%g,%g\n',t,E,L2rho,L2ux,L2uy);
    end
    
    if vis && mod(t,NVIS) == 0
        figure(1)
        clf
        
        umag = sqrt(u.*u+v.*v)/u_max;
        imagesc(x/NX,y/NY,umag)
        caxis([0 1])
        colormap(hot(1024))
        hcb = colorbar;
        ylabel(hcb,'|u|/u_{max}')
        
        hstr = streamslice(X/NX,Y/NY,u,v);
        set(hstr,'Color','white');
        
        axis xy equal tight
        xlabel('x/l_x')
        ylabel('y/l_y')
        
        
        td = 1/(nu*(2*pi/NX)^2 + (2*pi/NY)^2);
        title(['flow field at t/t_d = ' num2str(t/td)])
        drawnow
    end
end

% Calculate performance information after the simulation is finished
runtime = toc(tstart);
nodes_updated = NSTEPS*NX*NY;
speed = nodes_updated/(1e6*runtime);

fprintf(' ----- performance information -----\n');
fprintf('        timesteps: %u\n',NSTEPS);
fprintf('          runtime: %.3f (s)\n',runtime);
fprintf('            speed: %.2f (Mlups)\n',speed);

