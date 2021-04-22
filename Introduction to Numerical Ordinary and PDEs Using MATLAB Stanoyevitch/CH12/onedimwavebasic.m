function [x, t, U] = onedimwavebasic(phi, nu, L, A, B, T, N, M, c) 
% solves the one-dimensional wave problem u_tt = c(t,x,u,u_x)^2*u_xx
% Input variables:  phi=phi(x) = initial wave profile function 
% nu=nu(x) = initial wave velocity function, L = length of string, A =  % A(t) height function of left end of string u(0,t)=A(t), B=B(t) = 
% height function for right end of string u(L,t)=B, T= final time for  % which solution will be computed, N = number of internal x-grid  
% values,  M = number of internal t-grid values, c =c(t,x,u,u_x)= speed % of wave.  Functions of the indicated variables must be stored as
% (either inline or M-file) functions with the same variables, in the 
% same order.
% Output variables: t = time grid row vector (starts at t=0, ends at 
% t=T, has M+2 equally spaced values),  x = space grid row vector,  U =  % (M+2) by  (N+2) matrix of solution approximations at corresponding
% grid points; y grid will correspond to first (row) indices of U, x 
% grid values to second (column) indices of U. 
% CAUTION:  For stability of the method, the Courant-Friedrichs-Levy
% condition should hold:  c(x,t,u,u_x)(T/L)(N+1)/(M+1) <1.  

h = L/(N+1);, k = T/(M+1);, 
U=zeros(M+2,N+2); x=0:h:L;, t=0:k:T;
% Recall matrix indices must start at 1.  Thus the indices of the
% matrix will always be one more than the corresponding indices that
% were used in theoretical development.

%Assign left and right Dirichlet boundary values.
U(:,1)=feval(A,t)';, U(:,N+2)=feval(B,t)';

%Assign initial time t=0 values and next step t=k values.
for i=2:(N+1)
   U(1,i)=feval(phi,x(i));
   mu(i)=k*feval(c,0,x(i),U(1,i),(feval(phi,x(i+1))-feval(phi,x(i-1)))/2/h)/h;
   U(2,i)=U(1,i)+k*feval(nu,x(i));  
end

%Assign values at interior grid points
for j=3:(M+2)
for i=2:(N+1)
mu(i)=k*feval(c,t(j),x(i),U(j-1,i),(U(j-1,i+1)-U(j-1,i-1))/2/h)/h;
%First form needed tridiagonal matrix
Tri = diag(2*(1-mu(2:N+1).^2)) + diag(mu(3:N+1).^2, -1) + diag(mu(2:N).^2, 1);
%Now perform the matrix multiplications to iteratively obtain solution %values for increasing time levels.
    U(j,2:(N+1))=(Tri*(U(j-1,2:(N+1))'))'-U(j-2,2:(N+1));
    U(j,2)=U(j,2)+mu(2)^2*feval(A,t(j-1));
    U(j,N+1)=U(j,N+1)+mu(N+1)^2*feval(B,t(j-1));
end
end
