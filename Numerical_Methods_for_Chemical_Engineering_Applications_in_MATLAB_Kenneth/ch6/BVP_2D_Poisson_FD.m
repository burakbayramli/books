% BVP_2D_Poisson_FD.m
% This MATLAB routine solves a 2-D boundary value
% problem on a rectangular domain using finite differences.
% Zero Dirichlet boundary conditions are used.
% K. Beers. MIT ChE. 9/4/03
function iflag_main = ...
    BVP_2D_Poisson_FD(fun_name,L,H,num_pts);
iflag_main = 0;

% set number of points in grid
Nx = num_pts; Ny = num_pts;  Ntot = Nx*Ny;

% First, place a 2-D computational grid.
x = linspace(0,L,Nx); dx = x(2)-x(1);
y = linspace(0,H,Ny); dy = y(2)-y(1);

% Form a 2-D regular mesh such that
%     XG(i,j) = x(i), YG(i,j) = y(j)
[XG,YG] = form_2D_mesh(x,y);

% compute value of f(x,y) at each grid point
% and make a filled contour plot
FG = feval(fun_name,XG,YG,L,H);
figure; contourf(XG,YG,FG,min(Nx,Ny)); colorbar;
xlabel('x'); ylabel('y'); title('f(x,y)');

% allocate memory for the matrix and RHS vector
A = spalloc(Ntot,Ntot,5*Ntot); b = zeros(Ntot,1);

% We next specify equations for each boundary point.
% BC # 1
i = 1;
for j=1:Ny
    n = get_label(i,j,Nx,Ny);
    A(n,n) = 1;  b(n) = 0;
end
% BC # 2
i = Nx;
for j=1:Ny
    n = get_label(i,j,Nx,Ny);
    A(n,n) = 1;  b(n) = 0;
end
% BC # 3
j = 1;
for i=2:(Nx-1)
    n = get_label(i,j,Nx,Ny);
    A(n,n) = 1; b(n) = 0;
end
% BC # 4
j = Nx;
for i=2:(Nx-1)
    n = get_label(i,j,Nx,Ny);
    A(n,n) = 1; b(n) = 0;
end

% We now set the linear equations for the interior points.
factor_x = 1/(dx^2); factor_y = 1/(dy^2);
factor_cent = 2*(factor_x + factor_y);
for i=2:(Nx-1)
for j=2:(Ny-1)
    n = get_label(i,j,Nx,Ny);
    A(n,n-Ny) = -factor_x;  A(n,n+Ny) = -factor_x;
    A(n,n-1) = -factor_y;  A(n,n+1) = -factor_y;
    A(n,n) = factor_cent;
    b(n) = FG(i,j);
end
end

% We now solve using Gaussian elimination
phi = A\b;

% We now extract the results of the calculation
% into the 2-D grid format and make a filled
% contour plot.
PHIG = zeros(size(XG));
for i=1:Nx
for j=1:Ny
    n = get_label(i,j,Nx,Ny);
    PHIG(i,j) = phi(n);
end
end
figure; contourf(XG,YG,PHIG,min(Nx,Ny)); colorbar;
xlabel('x'); ylabel('y'); title('Poisson BVP (FD): \phi(x,y)');

% save the results to a .mat file
save BVP_2D_Poisson_FD.mat;

iflag_main = 1;
return;


%--------------------------------------
function [XG,YG] = form_2D_mesh(x,y);

Nx = length(x); Ny = length(y);
XG = zeros(Nx,Ny); YG = zeros(Nx,Ny);
for k=1:Nx
    XG(k,:) = x(k)*ones(1,Ny);
end
for k=1:Ny
    YG(:,k) = y(k)*ones(Nx,1);
end

return;


%--------------------------------------
function n = get_label(i,j,Nx,Ny);

n = (i-1).*Ny + j;

return;
