function [x,Y,ws,eigvals]=bvp2_eig(x0,xf,c0,cf,N)  
% use the finite difference method to solve an eigenvalue BVP4:
%  y"+w^2*y=0 with c01y(x0)+c02y'(x0)=0, cf1y(xf)+cf2y'(xf)=0
%input: x0/xf= the initial/final boundaries
%       c0/cf= the initial/final boundary condition coefficients 
%        N-1 = the number of internal grid points.
%output: x   = the vector of grid points 
%        Y   = the matrix composed of the eigenvector solutions
%        ws  = angular frequencies corresponding to eigenvalues
%    eigvals = the eigenvalues
if nargin<5|N<3, N=3; end
h=(xf-x0)/N; h2=h*h; x=x0+[0:N]*h;
N1=N+1;
if abs(c0(2))<eps, N1=N1-1; A(1,1:2)=[2 -1]; 
 else A(1,1:2)=[2*(1-c0(1)/c0(2)*h) -2];             %(P6.11-4a)
end
if abs(cf(2))<eps, N1=N1-1; A(N1,N1-1:N1)=[-1 2]; 
 else A(N1,N1-1:N1)=[-2 2*(1+cf(1)/cf(2)*h)];        %(P6.11-4c)
end 
if N1>2
  for m=2:ceil(N1/2), A(m,m-1:m+1)=[-1 2 -1]; end    %(P6.11-4b)      
end
for m=ceil(N1/2)+1:N1-1, A(m,:)=fliplr(A(N1+1-m,:)); end 
[V,LAMBDA]=eig(A); eigvals=diag(LAMBDA)';
[eigvals,I]=sort(eigvals); % sorting in the ascending order
V=V(:,I);
ws=sqrt(eigvals)/h;
if abs(c0(2))<eps, Y=zeros(1,N1); else Y=[]; end
Y=[Y; V];
if abs(cf(2))<eps, Y=[Y; zeros(1,N1)]; end
