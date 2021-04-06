%%LID DRIVEN CAVITY FLOW EXAMPLE
%%The code is based on the SIMPLE algorithm

clc
clear all
close all
format longG

%% GRID SIZE AND OTHER PARAMETERS
%i runs along x-direction and j runs along y-direction 

imax=33;                        %grid size in x-direction 
jmax=33;                        %grid size in y-direction 
max_iteration=6000; 
maxRes = 1000;
iteration = 1;
mu = 0.01;                      %viscosity
rho = 1;                        %density
velocity=1;                     %velocity = lid velocity
dx=1/(imax-1);					%dx,dy cell sizes along x and y directions
dy=1/(jmax-1); 
x=dx/2:dx:1-dx/2; 
y=0:dy:1; 
alphaP = 0.1;                   %pressure under-relaxation
alphaU = 0.7;                   %velocity under-relaxation
tol = 1e-5;

%   u_star, v_star are Intermediate velocities
%   u and v = Final velocities

%Variable declaration
p   = zeros(imax,jmax);             %   p = Pressure
p_star   = zeros(imax,jmax);        
p_prime = zeros(imax,jmax);         %   pressure correction 
rhsp = zeros(imax,jmax);            %   Right hand side vector of pressure correction equation
divergence = zeros(imax,jmax); 

%Vertical velocity
v_star = zeros(imax,jmax+1);
vold   = zeros(imax,jmax+1);
vRes   = zeros(imax,jmax+1);
v      = zeros(imax,jmax+1);
d_v    = zeros(imax,jmax+1);    %velocity orrection coefficient

% Horizontal Velocity -----------
u_star = zeros(imax+1,jmax);
uold   = zeros(imax+1,jmax);
uRes   = zeros(imax+1,jmax);
u      = zeros(imax+1,jmax);
d_u    = zeros(imax+1,jmax);  %velocity orrection coefficient

%Boundary condition 
%Lid velocity (Top wall is moving with 1m/s)
u_star(1:imax+1,jmax)=velocity;
u(1:imax+1,jmax)=velocity;

%% ---------- iterations -------------------
while ( (iteration <= max_iteration) && (maxRes > tol) ) 
    iteration = iteration + 1;
    [u_star,d_u] = u_momentum(imax,jmax,dx,dy,rho,mu,u,v,p_star,velocity,alphaU);       %%Solve u-momentum equation for intermediate velocity u_star 
    [v_star,d_v] = v_momentum(imax,jmax,dx,dy,rho,mu,u,v,p_star,alphaU);                 %%Solve v-momentum equation for intermediate velocity v_star
    uold = u;
    vold = v; 
    [rhsp] = get_rhs(imax,jmax,dx,dy,rho,u_star,v_star);                                 %%Calculate rhs vector of the Pressure Poisson matrix 
    [Ap] = get_coeff_mat_modified(imax,jmax,dx,dy,rho,d_u,d_v);                          %%Form the Pressure Poisson coefficient matrix 
    [p,p_prime] = pres_correct(imax,jmax,rhsp,Ap,p_star,alphaP);                         %%Solve pressure correction implicitly and update pressure
    [u,v] = updateVelocity(imax,jmax,u_star,v_star,p_prime,d_u,d_v,velocity);            %%Update velocity based on pressure correction
    [divergence]=checkDivergenceFree(imax,jmax,dx,dy,u,v);                               %%check if velocity field is divergence free
    p_star = p;                                                                          %%use p as p_star for the next iteration
    
    %find maximum residual in the domain
    vRes = abs(v - vold);
    uRes = abs(u - uold);
    maxRes_u = max(max(uRes));
    maxRes_v = max(max(vRes));
    maxRes = max(maxRes_u, maxRes_v);
                                                                            %%Check for convergence 
    disp(['It = ',int2str(iteration),'; Res = ',num2str(maxRes)])
    if (maxRes > 2)
        disp('not going to converge!');
        break;
    end
end
%% plot


disp(['Total Iterations = ',int2str(iteration)])

figure 
contourf(x,y,u(2:imax,:)',50, 'edgecolor','none');colormap jet
colorbar;
axis([0 1 0 1]); 
title('steady Ux'); 



