% BVP_2D_Poisson_FD_cg.m
% This MATLAB routine solves a 2-D boundary value
% problem on a rectangular domain using finite differences.
% Zero Dirichlet boundary conditions are used.
% This program uses the conjugate gradient method to solve
% the linear system, and does not even store in memory
% the matrix of the problem.
% K. Beers. MIT ChE. 10/17/03
function iflag_main = ...
    BVP_2D_Poisson_FD_cg(fun_name,L,H,num_pts);
iflag_main = 0;

% set number of points in grid
Nx = num_pts; Ny = num_pts;  Ntot = Nx*Ny;

% First, place a 2-D computational grid.
x = linspace(0,L,Nx);  y = linspace(0,H,Ny);

% Form a 2-D regular mesh such that
%     XG(i,j) = x(i), YG(i,j) = y(j)
[XG,YG] = form_2D_mesh(x,y);

% compute value of f(x,y) at each grid point
FG = feval(fun_name,XG,YG,L,H);

% set RHS vector of linear system
b = zeros(Ntot,1);
% b only has non-zero values for interior points
for i=2:(Nx-1)
for j=2:(Ny-1)
    n = get_label(i,j,Nx,Ny);
    b(n) = FG(i,j);
end
end

% Now, solve the program using the conjugate gradient method,
% where we suppy a routine Poisson_2D_calc_Av() that
% multiplies an input vector v by the matrix A. We supply
% the name of this routine in lieu of storing the matrix
% in memory.
CG_iter_max = round(1.1*Ntot);  tol = 1e-6;
phi = pcg(@Poisson_2D_calc_Av,b,tol,CG_iter_max,[],[],zeros(Ntot,1),x,y);

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
save BVP_2D_Poisson_FD_cg.mat;

iflag_main = 1;
return;


%--------------------------------------
% This routine returns the product of the A matrix with an
% input vector v. It is used in the CG solver in lieu of
% storing the matrix A explicitly.
function Av = Poisson_2D_calc_Av(v,x,y);
Av = zeros(size(v));
Nx =length(x);  dx = x(2) - x(1);
Ny = length(y);  dy = y(2) - y(1);

% We next specify equations for each boundary point.
% BC # 1
i = 1;
for j=1:Ny
    n = get_label(i,j,Nx,Ny);
    Av(n) = Av(n) + v(n);
end
% BC # 2
i = Nx;
for j=1:Ny
    n = get_label(i,j,Nx,Ny);
    Av(n) = Av(n) + v(n);
end
% BC # 3
j = 1;
for i=2:(Nx-1)
    n = get_label(i,j,Nx,Ny);
    Av(n) = Av(n) + v(n);
end
% BC # 4
j = Nx;
for i=2:(Nx-1)
    n = get_label(i,j,Nx,Ny);
    Av(n) = Av(n) + v(n);
end

% We now set the linear equations for the interior points.
factor_x = 1/(dx^2); factor_y = 1/(dy^2);
factor_cent = 2*(factor_x + factor_y);
for i=2:(Nx-1)
for j=2:(Ny-1)
    n = get_label(i,j,Nx,Ny);
    Av(n) = Av(n) - factor_x*v(n-Ny) - factor_x*v(n+Ny);
    Av(n) = Av(n) - factor_y*v(n-1) - factor_y*v(n+1);
    Av(n) = Av(n) + factor_cent*v(n);
end
end

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
