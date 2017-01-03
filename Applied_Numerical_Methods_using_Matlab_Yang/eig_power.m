function [lambda,v]=eig_power(A,x,EPS,MaxIter)
% The power method to find the largest eigenvalue (lambda) and 
% the corresponding eigenvector (v) of a matrix A.
if nargin<4, MaxIter=100; end % maximum number of iterations
if nargin<3, EPS=1e-8; end % difference between successive values
N= size(A,2); 
if nargin<2, x=[1:N]; end % the initial vector
x= x(:);
lambda=0; 
for k=1:MaxIter
  x1= x;  lambda1= lambda;
  x= A*x/norm(x,inf); %Eq.(8.3-4)
  [xm,m]= max(abs(x));
  lambda= x(m); % the component with largest magnitude(absolute value)
  if norm(x1-x)<EPS&abs(lambda1-lambda)<EPS, break; end
end 
if k==MaxIter, disp('Warning: you may have to increase MaxIter'); end
v= x/norm(x);
