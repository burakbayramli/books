% This code accompanies
%   The Lattice Boltzmann Method: Principles and Practice
%   T. Krüger, H. Kusumaatmaja, A. Kuzmin, O. Shardt, G. Silva, E.M. Viggen
%   ISBN 978-3-319-44649-3 (Electronic) 
%        978-3-319-44647-9 (Print)
%   http://www.springer.com/978-3-319-44647-9
%
% This code is provided under the MIT license. See LICENSE.txt.
%
% Author: Goncalo Silva
% 
% Example matlab code for computing a Couette flow with BB
% Solves problems from Section 5.3.3.6 in book

clear all
close all
clc

% simulation parameters
scale=1;                % set simulation size
NX=3*scale;             % channel length
NY=5*scale;             % channel width
NSTEPS=1e4*scale^2;     % number of simulation time steps
tau=0.9;                % relaxation time (BGK model)
omega=1/tau;
u_max=0.1/scale;        % maximum velocity
nu=(2*tau-1)/6;         % kinematic shear viscosity
Re=NY*u_max/nu;         % Reynolds number; scaling parameter in simulation


% Lattice parameters; note zero direction is last
NPOP=9;                                         % number of velocities
w  = [1/9 1/9 1/9 1/9 1/36 1/36 1/36 1/36 4/9]; % weights
cx = [1 0 -1  0 1 -1 -1  1 0];                  % velocities, x components
cy = [0 1  0 -1 1  1 -1 -1 0];                  % velocities, y components


% Node locations
x = (1:NX)-0.5;
y = (1:NY)-0.5;

% Analytical solution: Couette velocity
u_analy=u_max/NY.*y;

% initialize populations
feq=zeros(NX,NY,NPOP);
for k=1:NPOP
    feq(:,:,k)=w(k); % assuming density equal one and zero velocity initial state
end
f=feq;
fprop=feq;

% convergence parameters
tol=1e-12;      % tolerance to steady state convergence
teval=100;      % time step to evaluate convergence
u_old=zeros(NX,NY);

% initalize clock
tstart = tic;

% Main algorithm
for t=1:NSTEPS
    % Compute macroscopic quantities
    % density
    rho = sum(fprop,3);
    
    % momentum components
    u = sum(fprop(:,:,[1 5 8]),3) - sum(fprop(:,:,[3 6 7]),3);
    v = sum(fprop(:,:,[2 5 6]),3) - sum(fprop(:,:,[4 7 8]),3);
    
    % check convergence
    if mod(t,teval)==1
        
        conv = abs(mean(u(:))/mean(u_old(:))-1);
        
        if conv<tol
            break
        else
            u_old = u;
        end
    end
    
    
    for k=1:NPOP
        % Compute equilibrium distribution (linear equilibrium with incompressible model)
        feq(:,:,k)=w(k)*(rho + 3*(u*cx(k)+v*cy(k)));
    end
    % Collision step
    f = (1-omega)*fprop + omega*feq;
    
    for k=1:NPOP
        for j=1:NY
            for i=1:NX
                
                % Streaming step (Periodic streaming of whole domain)
                newx=1+mod(i-1+cx(k)+NX,NX);
                newy=1+mod(j-1+cy(k)+NY,NY);
                fprop(newx,newy,k)=f(i,j,k);
                
            end
        end
    end
    
    
    % Boundary condition (bounce-back)
    % Top wall (moving with tangential velocity u_max)
    fprop(:,NY,4)=f(:,NY,2);
    fprop(:,NY,7)=f(:,NY,5)-(1/6)*u_max;
    fprop(:,NY,8)=f(:,NY,6)+(1/6)*u_max;
    
    % Bottom wall (rest)
    fprop(:,1,2)=f(:,1,4);
    fprop(:,1,5)=f(:,1,7);
    fprop(:,1,6)=f(:,1,8);
end



% Calculate performance information after the simulation is finished
runtime = toc(tstart);

% Compute error: L2 norm
error=zeros(NX,1);
for i=1:NX
    error(i)=(sqrt(sum((u(i,:)-u_analy).^2)))./sqrt(sum(u_analy.^2));
end
L2=1/NX*sum(error);

% Accuracy information
fprintf(' ----- accuracy information -----\n');
fprintf('        L2(u): %g\n',L2);

