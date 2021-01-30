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
% Example matlab code for computing a Poiseuille flow with wetnode BC
% and different LB body force models
% Solves problems Section 6.7 (Benchmark Problems) in book 
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
tau=0.7;                % relaxation time (BGK model) (tau=1 gives exact solution)
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

% Wall locations
ybottom=1;
ytop=NY;

% Select type of wetnode BC: 
% 1) Equilibrium scheme:                        wetnode=1 
% 2) Non-equilibrium extrapolation method:      wetnode=2 
% 3) Non-equilibrium bounce-back method:        wetnode=else 
wetnode=0;

% Force density
Fx=8*nu*u_max/(ytop-ybottom)^2;
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
u_analy=-4*u_max/((ytop-ybottom)^2).*(y-ybottom).*(y-ytop);


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
    
    
    % Boundary condition (wet node)
    
    % Setting macroscopic quantities at boundaries
   
    % Bottom wall (rest)
    rho(:,1)=1/(1-v(:,1))*(fprop(:,1,9)+fprop(:,1,1)+fprop(:,1,3)+...
        2*(fprop(:,1,4)+fprop(:,1,7)+fprop(:,1,8))+1/2*forcey(:));
    u(x,1)=0;
    v(x,1)=0;
    
    % Top wall (rest)
    rho(:,NY)=1/(1+v(:,NY))*(fprop(:,NY,9)+fprop(:,NY,1)+fprop(:,NY,3)+...
        2*(fprop(:,NY,2)+fprop(:,NY,5)+fprop(:,NY,6))-1/2.*forcey(:));
    u(x,NY)=0;
    v(x,NY)=0;

    % Setting populations quantities at boundaries (note: rho=1)
    % non-equilibrium bounce-back method BC 
    fprop(:,1,2)=fprop(:,1,4)+2/3*v(:,1)-1/6*forcey(:);
    fprop(:,1,5)=fprop(:,1,7)+1/6*v(:,1)-0.5*(fprop(:,1,1)-fprop(:,1,3))+1/2*u(:,1)-1/4*forcex(:)-1/6*forcey(:);
    fprop(:,1,6)=fprop(:,1,8)+1/6*v(:,1)+0.5*(fprop(:,1,1)-fprop(:,1,3))-1/2*u(:,1)+1/4*forcex(:)-1/6*forcey(:);
    
    fprop(:,NY,4)=fprop(:,NY,2)-2/3*v(:,NY)+1/6*forcey(:);
    fprop(:,NY,7)=fprop(:,NY,5)-1/6*v(:,NY)+0.5*(fprop(:,NY,1)-fprop(:,NY,3))-1/2*u(:,NY)+1/4*forcex(:)+1/6*forcey(:);
    fprop(:,NY,8)=fprop(:,NY,6)-1/6*v(:,NY)+0.5*(fprop(:,NY,3)-fprop(:,NY,1))+1/2*u(:,NY)-1/4*forcex(:)+1/6*forcey(:);
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

