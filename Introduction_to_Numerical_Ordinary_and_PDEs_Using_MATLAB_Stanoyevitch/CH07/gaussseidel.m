function [x, k, diff] = gaussseidel(A,b,x0,tol,kmax)
% performs the Gauss-Seidel iteration on the linear system Ax=b.  
% Inputs:  the coefficient matrix 'A', the inhomogeneity (column) vector 'b', 
% the seed (column) vector 'x0' for the iteration process, the tolerance 'tol' 
% which will cause the iteration to stop if the 2-norms of differences of successive iterates
% becomes smaller than 'tol', and 'kmax' which it the maximum number of iterations
% to perform.  
% Outputs:  the final iterate 'x', the number of iterations performed 'k', 
% and a vector 'diff' which records the 2-norms of successive differences of
% iterates.  
% If any of the last last three input variables are not specified, 
% default values of x0= zero column vector, tol=1e-10 and kmax=100 are used.  


%assign default input variables, as necessary
if nargin<3, x0=zeros(size(b));, end
if nargin<4, tol=1e-10;, end
if nargin<5, kmax=100;, end

if min(abs(diag(A)))<eps
    error('Coefficient matrix has zero diagonal entries, iteration cannot be performed.\r')
end

[n m]=size(A);
xold=x0;
k=1;, diff=[];

while k<=kmax
    xnew=b;
    for i=1:n
        for j=1:n
            if j<i
                xnew(i)=xnew(i)-A(i,j)*xnew(j);
            elseif j>i
                xnew(i)=xnew(i)-A(i,j)*xold(j);
            end
        end
        xnew(i)=xnew(i)/A(i,i);
    end
    diff(k)=norm(xnew-xold,2); 
    if diff(k)<tol
            fprintf('Gauss Seidel iteration has converged in %d iterations\r', k)
            x=xnew;
            return
    end
    k=k+1;, xold=xnew;
end
fprintf('Gauss Seidel iteration failed to converge.\r')
x=xnew;
    
            
