function [x, y, t, U] = twodimwavedirbc(phi, nu, a, b, T, h, c) 
% solves the two-dimensional wave problem u_tt = c^2(u_xx+u_yy)
% on the rectangle {0<= x <= a,  0<= y <= b}, with u(x,y)=0
% on the boundary of the rectangle.  
% Input variables:  phi =initial wave profile function 
% nu initial wave velocity function, both should be functions of 
% (x,y).  a= right endpoint of x, b = right endpoint of y,  
% T = final time solution will be computed.  h=common gap on 
% x, y-grids,   k= t-grid gap, c = speed of wave.
% wave.
% Output variables: x = row vector for first space variable, y =
% row vector for second space variable,  t = time grid row vector 
%(starts at t=0, ends at t=T, has Nt equally spaced values),  
% U = (Nx)by(Ny)by(Nt) matrix of solution approximations at 
% corresponding grid points (where Ny = number of y-grid points)
% x grid will correspond to first entries of U, y 
% grid values to second entries of U, and t grid to third entries of U.
% CAUTION:  This method will only work if h is chosen so that the x 
% and y grids can have a common gap size, i.e., if h = a/h and 
% b/h must be integers. 
% The time grid gap is chosen so that mu = 1/2, this guarantees the 
% Courant-Friedrichs-Levy condition holds and simplifies the 
% main finite difference formula.

k=h/sqrt(2)/c; %k is determined from mu = 1/2
if (b/h-floor(round(b/h))>10*eps)|(a/h-floor(round(a/h))>10*eps)
fprintf('Space grid gap h must divide evenly into both a and b \r')
fprintf(' Either try another input or modify the algorithm')
error('M-file will exit')
end
Nx = round(a/h)+1; %number of points on x-grid
Ny = round(b/h)+1; %number of points on y-grid
Nt = floor(T/k)+1; %number of points on t-grid
U=zeros(Nx, Ny, Nt); x=0:h:a;, y=0:h:b;, t=0:k:T;
% Recall matrix indices must start at 1.  Thus the indices of the
% matrix will always be one more than the corresponding indices that
% were used in theoretical development.

% Note that by default, zero boundary values have been assigned to all
% grid points on the edges of the rectangle (and, for the time being,
% at all grid points).



%Assign initial time t=0 values and next step t=k values.
for i=2:(Nx-1)
     for j=2:(Ny-1)
    U(i,j,1)=feval(phi,x(i),y(j));
    U(i,j,2)=.25*(feval(phi,x(i-1),y(j))+...
      feval(phi,x(i+1),y(j))+feval(phi,x(i),y(j-1))+...
      feval(phi,x(i),y(j+1)))+ k*feval(nu,x(i),y(j)); 
     
end
end
%Assign values at interior grid points
for ell=3:Nt %letter ell looks too much like number one


    U(2:(Nx-1),2:(Ny-1), ell) = ...
     +.5*(U(3:Nx,2:(Ny-1), ell-1)+ U(1:(Nx-2),2:(Ny-1), ell-1)...
     + U(2:(Nx-1),3:Ny, ell-1) + U(2:(Nx-1),1:(Ny-2), ell-1))...
     - U(2:(Nx-1),2:(Ny-1), ell-2);
end

