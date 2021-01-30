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
% Example matlab code for computing a Couette flow with wetnode BC
% Solves problems from Section 5.3.4 in book

clear all
close all
clc

% simulation parameters
scale=1;                % set simulation size
NX=3*scale;             % channel length
NY=5*scale;             % channel width
NSTEPS=1e4*scale^2;     % number of simulation time steps
tau=0.8;                % relaxation time (BGK model)
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
x = (1:NX);
y = (1:NY);

% Select type of wetnode BC: 
% 1) Equilibrium scheme:                        wetnode=1 
% 2) Non-equilibrium extrapolation method:      wetnode=2 
% 3) Non-equilibrium bounce-back method:        wetnode=else 
wetnode=3;

% Analytical solution: Couette velocity
u_analy=u_max/(NY-1).*(y-1);

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
    
    
    % Boundary condition (wet node)
    
    % Setting macroscopic quantities at boundaries
   
    % Bottom wall (rest)
    rho(:,1)=1/(1-v(:,1))*(fprop(:,1,9)+fprop(:,1,1)+fprop(:,1,3)+...
        2*(fprop(:,1,4)+fprop(:,1,7)+fprop(:,1,8)));
    u(x,1)=0;
    v(x,1)=0;
    
    % Top wall (moving)
    rho(:,NY)=1/(1+v(:,NY))*(fprop(:,NY,9)+fprop(:,NY,1)+fprop(:,NY,3)+...
        2*(fprop(:,NY,2)+fprop(:,NY,5)+fprop(:,NY,6)));
    u(x,NY)=u_max;
    v(x,NY)=0;

    % Setting populations quantities at boundaries
    if wetnode==1         % 1) equilibrium scheme BC
        for k=1:NPOP
            fprop(:,1,k)=w(k)*(rho(:,1)+3*(cx(k)*u(:,1)+cy(k)*v(:,1)));
            fprop(:,NY,k)=w(k)*(rho(:,NY)+3*(cx(k)*u(:,NY)+cy(k)*v(:,NY)));
        end
    elseif wetnode==2      % 2) non-equilibrium extrapolation method BC
        for k=1:NPOP    
            fprop(:,1,k)=w(k)*(rho(:,1)+3*(cx(k)*u(:,1)+cy(k)*v(:,1)))+(fprop(:,2,k)-feq(:,2,k));
            fprop(:,NY,k)=w(k)*(rho(:,NY)+3*(cx(k)*u(:,NY)+cy(k)*v(:,NY)))+(fprop(:,NY-1,k)-feq(:,NY-1,k));
        end
    else                   % 3) non-equilibrium bounce-back method BC (note: rho=1)
        fprop(:,1,2)=fprop(:,1,4)+2/3*v(:,1);
        fprop(:,1,5)=fprop(:,1,7)+1/6*v(:,1)-0.5*(fprop(:,1,1)-fprop(:,1,3))+1/2*u(:,1);
        fprop(:,1,6)=fprop(:,1,8)+1/6*v(:,1)+0.5*(fprop(:,1,1)-fprop(:,1,3))-1/2*u(:,1);
        
        fprop(:,NY,4)=fprop(:,NY,2)-2/3*v(:,NY);
        fprop(:,NY,7)=fprop(:,NY,5)-1/6*v(:,NY)+0.5*(fprop(:,NY,1)-fprop(:,NY,3))-1/2*u(:,NY);
        fprop(:,NY,8)=fprop(:,NY,6)-1/6*v(:,NY)+0.5*(fprop(:,NY,3)-fprop(:,NY,1))+1/2*u(:,NY);
    end
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

