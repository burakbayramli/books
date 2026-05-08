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
% Example matlab code for computing a Poiseuille flow with BB 
% and different LB body force models
% Solves problems from Section 6.7 (Benchmark Problems) in book
% which were inspired in publications: 
% http://dx.doi.org/10.1016/j.physa.2010.11.037
% http://dx.doi.org/10.1017/jfm.2012.83 

clear all
close all
clc

% simulation parameters
scale=1;                % set simulation size
NX=5*scale;             % channel length
NY=5*scale;             % channel width
NSTEPS=1e4*scale^2;     % number of simulation time steps
tau=sqrt(3/16)+0.5;     % relaxation time (BGK model) (tau=sqrt(3/16)+0.5 gives exact solution)
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

% Force density
Fx=8*nu*u_max/NY^2;
% Pressure gradient
gradP=Fx;

% Select LB force model based on order of discretisation
% First order discretisation in time/configuration space:                  force_spatial_order=1
% Second order discretisation in time/configuration space:                 force_spatial_order=2
% First order discretisation in velocity space (Buick and Greated):        force_velocity_order=1
% Second order discretisation in velocity space (Guo et al.):              force_velocity_order=2
force_spatial_order=2;
force_velocity_order=1; 


% Select type of density/pressure gradient 
% 1) Constant body force and Zero pressure gradient:                        select_combination=1 
% 2) Constant body force (50%) and Constant pressure gradient (50%):        select_combination=2 
% 3) Linearly varying body force (50%) and Linearly varying pressure gradient (50%):select_combination=else 
select_combination=3;

if select_combination==1 % Constant body force and Zero pressure gradient
    
    forcex=ones(1,NX)*Fx;
    forcey=zeros(1,NX);
    gradP=0;
    
elseif select_combination==2 % Constant body force (50%) and Constant pressure gradient (50%)
    
    forcex=ones(1,NX)*Fx*1/2;
    forcey=zeros(1,NX);
    gradP=gradP*1/2;
    
else % Linearly varying body force (50%) and Linearly varying pressure gradient (50%)
    
    forcex=zeros(1,NX);
    for i=1:NX
        forcex(i)=Fx.*i./(NX+1) ;
    end
    forcey=zeros(1,NX);
    gradP=gradP*1/2;
end

% Pressure conditions
rho_outlet=1;
rho_inlet=3*(NX-1)*gradP+rho_outlet;

% Analytical solution: Poiseuille velocity
ybottom=0;
ytop=NY;
u_analy=-4*u_max/(NY^2).*(y-ybottom).*(y-ytop);


% initialize populations
feq=zeros(NX,NY,NPOP);
for k=1:NPOP
    for i=1:NX
         % assuming density equal one and zero velocity initial state
        feq(i,:,k)=w(k)-0.5*w(k)*3*(cx(k)*forcex(i)+cy(k)*forcey(i)); 
    end
end
f=feq;
fprop=feq;
F=zeros(NX,NY,NPOP);

% Initialize macroscopic quantities
u=zeros(NX,NY);
v=zeros(NX,NY);

% convergence parameters
tol=1e-12;      % tolerance to steady state convergence
teval=100;      % time step to evaluate convergence
u_old=u;

% initalize clock
tstart = tic;

% Main algorithm
for t=1:NSTEPS
    % Compute macroscopic quantities
    % density
    rho = sum(fprop,3);
    
    % momentum components
    if force_spatial_order==1
        u = sum(fprop(:,:,[1 5 8]),3) - sum(fprop(:,:,[3 6 7]),3);
        v = sum(fprop(:,:,[2 5 6]),3) - sum(fprop(:,:,[4 7 8]),3);
    elseif force_spatial_order==2 % considers half-force correction
        for i=1:NX
            u(i,:) = sum(fprop(i,:,[1 5 8]),3) - sum(fprop(i,:,[3 6 7]),3)+forcex(i)/2;
            v(i,:) = sum(fprop(i,:,[2 5 6]),3) - sum(fprop(i,:,[4 7 8]),3)+forcey(i)/2;
        end
    end
    
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
        
        % Forcing computation
        if force_spatial_order==1
            
            if force_velocity_order==1
                for i=1:NX
                    F(i,:,k)=w(k)*3*(cx(k)*forcex(i)+cy(k)*forcey(i));
                end
            elseif force_velocity_order==2
                for i=1:NX
                    F(i,:,k)=w(k)*...
                        ((3*(cx(k)-u(i,:))+9*(cx(k)*u(i,:)+cy(k)*v(i,:))*cx(k))*forcex(i)...
                        +(3*(cy(k)-v(i,:))+9*(cx(k)*u(i,:)+cy(k)*v(i,:))*cy(k))*forcey(i));
                end
            end
            
        elseif force_spatial_order==2 % includes information of relaxation in forcing scheme
            
            if force_velocity_order==1
                for i=1:NX
                    F(i,:,k)=(1-omega/2)*w(k)*3*(cx(k)*forcex(i)+cy(k)*forcey(i));
                end
            elseif force_velocity_order==2
                for i=1:NX
                    F(i,:,k)=(1-omega/2)*w(k)*...
                        ((3*(cx(k)-u(i,:))+9*(cx(k)*u(i,:)+cy(k)*v(i,:))*cx(k))*forcex(i)...
                        +(3*(cy(k)-v(i,:))+9*(cx(k)*u(i,:)+cy(k)*v(i,:))*cy(k))*forcey(i));
                end
            end
        end
        
    end
    % Collision step
    f = (1-omega)*fprop + omega*feq + F;
    
    % Inlet/Outlet BC: PBBC (w/ i=1 and i=NX outside layers)    
    for k=1:NPOP
        f(1,:,k) = w(k)*(rho_inlet+...
            3*(cx(k)*u(NX-1,:)+cy(k)*v(NX-1,:)))...
             +(f(NX-1,:,k)-feq(NX-1,:,k));
        f(NX,:,k)= w(k)*(rho_outlet+...
            3*(cx(k)*u(2,:)+cy(k)*v(2,:)))...
            +(f(2,:,k)-feq(2,:,k));       
    end
    
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
    % Top wall (rest)
    fprop(:,NY,4)=f(:,NY,2);
    fprop(:,NY,7)=f(:,NY,5);
    fprop(:,NY,8)=f(:,NY,6);
    
    % Bottom wall (rest)
    fprop(:,1,2)=f(:,1,4);
    fprop(:,1,5)=f(:,1,7);
    fprop(:,1,6)=f(:,1,8);
end

% Calculate performance information after the simulation is finished
runtime = toc(tstart);

% Compute error: L2 norm
%(note i=1 and i=NX are virtual, not fluid, layers; thus not considered in error)
error=zeros(NX,1);
for i=2:NX-1
    error(i)=(sqrt(sum((u(i,:)-u_analy).^2)))./sqrt(sum(u_analy.^2));
end
L2=1/NX*sum(error);

% Accuracy information
fprintf(' ----- accuracy information -----\n');
fprintf('        L2(u): %g\n',L2);

