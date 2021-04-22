function [x, t, U] = cranknicolson(phi, L, A, B, T, N, M, alpha,q) 
% solves the one dimensional heat problem 
% u_t = alpha(t,x,u)*u_xx+q(x,t)
% using the Crank-Nicolson method. 
% Input variables:  phi=phi(x) = initial wave profile function 
% L = length of rod, A =A(t)= temperature of left end of rod 
% u(0,t)=A(t), B=B(t) = temperature of right end of rod u(L,t)=B(t), 
% T= final time for which solution will be 
% computed, N = number of internal x-grid values,  M = number
% of internal t-grid values, alpha =alpha(t,x,u)= diffusivity of rod.
% q = q(x,t) = internal heat source function
% Output variables: t = time grid row vector (starts at t=0, ends at 
% t=T, has M+2 equally spaced values),  x = space grid row vector,  
% U = (M+2) by (N+2) matrix of solution approximations at corresponding
% grid points. x grid will correspond to second (column)entries of U, y 
% grid values to first (row) entries of U. Row 1 of U corresponds to 
% t = 0.

h = L/(N+1);, k = T/(M+1);,  
U=zeros(M+2,N+2); x=0:h:L;, t=0:k:T;

% Recall matrix indices must start at 1.  Thus the indices of the
% matrix will always be one more than the corresponding indices that
% were used in theoretical development.

%Assign left and right Dirichlet boundary values.
U(:,1)=feval(A,t)';, U(:,N+2)=feval(B,t)';

%Assign initial time t=0 values.
for i=2:(N+1)
    U(1,i)=feval(phi,x(i)); 
end

%Assign values at interior grid points
for j=2:(M+2)
for i=2:(N+1)
mu(i)=k*feval(alpha,t(j-1),x(i),U(j-1,i))/h^2;
mu2(i)=k*feval(alpha,t(j),x(i),U(j-1,i))/h^2;
q1(i)=feval(q,x(i),t(j-1));, q2(i)=feval(q,x(i),t(j));
end

% First form needed vectors and matrices, because we will be using the
% thomas M-file, we do not need to construct the coefficient matrix T.

S = diag(2*(1-mu2(2:N+1))) + diag(mu2(3:N+1), -1) + diag(mu(2:N), 1);
V = zeros(N,1);, V(1)=mu(2)*U(j-1,1)+mu2(2)*U(j,1);, 
V(N)=mu(N+1)*U(j-1,N+2)+mu2(N+1)*U(j,N+2);
Q = k*(q1(2:N+1)+q2(2:N+1))';

%Now perform the matrix multiplications to iteratively obtain solution 
% values for increasing time levels.
c=S*((U(j-1,2:(N+1)))')+V+Q;
a=-mu2(2:N+1);, b=a;, a(N)=0; b(1)=0;
U(j,2:N+1)=thomas(a,2*(1+mu2(2:N+1)),b,c);
end

