% Driver script for solving the 2D Euler equations using a WENO scheme
clear all

% Order of method
m=2;

% Set problem parameters for isentropic vortex
% Lx = 10; Ly = 10; Nx = 64; Ny = 64; FinalTime = 1.0; CFL = 0.5; gamma= 1.4;
% hx = Lx/Nx; hy = Ly/Ny; 

% Set up Legendre quadrature grid for cell average
% NGQ = 10; [xGQ,wGQ] = LegendreGQ(NGQ);
% xQ1 = hx/2*xGQ; yQ1 = hy/2*xGQ; [xQ,yQ] = meshgrid(xQ1,yQ1);

% Define domain and initial conditions
% xv = [-5:hx:5]; yv = [-5:hy:5]; [x,y] = meshgrid(xv,yv);
% r = zeros(Ny+1,Nx+1); ru = zeros(Ny+1,Nx+1); 
% rv = zeros(Ny+1,Nx+1); E = % zeros(Ny+1,Nx+1);
% x0=0.0; y0=0.0; u0=1.0; v0=0; beta=5.0;
% for j=1:Nx+1
%     for i=1:Ny+1
%         xL = xQ + xv(j); yL = yQ + yv(i);
%         [rL,ruL,rvL,EL] = ...
%              IsentropicVortex2D(xL,x0,u0,yL,y0,v0,gamma,beta,0.0);
%        r(i,j) = wGQ'*rL*wGQ/4; ru(i,j) = wGQ'*ruL*wGQ/4;
%        rv(i,j) = wGQ'*rvL*wGQ/4; E(i,j) = wGQ'*EL*wGQ/4;
%    end
% end

% Set problem parameters for 2D Riemann problems
Lx = 1; Ly = 1; Nx = 99; Ny = 99; CFL = 0.5; gamma= 1.4; 
RiemannProbCase = 4;
hx = Lx/Nx; hy = Ly/Ny; 

% Define domain and initial conditions
xv = [0:hx:Lx]; yv = [0:hy:Ly]; [x,y] = meshgrid(xv,yv);
[r,ru,rv,E,FinalTime] = Riemann2D(x,y,gamma,RiemannProbCase);

% Solve Problem
q = zeros(Ny+1,Nx+1,4);
q(:,:,1) = r; q(:,:,2)=ru; q(:,:,3)=rv; q(:,:,4)=E; 
[q] = EulerWENO2D(x,y,q,hx,hy,m,gamma,CFL,FinalTime);