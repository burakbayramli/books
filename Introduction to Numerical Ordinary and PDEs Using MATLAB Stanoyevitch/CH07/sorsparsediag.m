function [x, k, diff] = sorsparsediag(diags, inds,b,omega, x0,tol,kmax)
% performs the SOR iteration on the linear system Ax=b. 
% in case where the nbyn coefficient matrix A has entries only on a
% sparse set of diagonals. 
% Inputs:  The input variables are 'diags', an nbyJ matrix where each
% column consists of the entries of on one of A's diagonals.  The first 
% column of diags is the main diagonal of A (even if all zeros)
% and 'inds' , a 1byn vector of the corresponding set of indices for 
% the diagonals (index zero corresponds to the main diagonal). 
% the relaxation paramter 'omega', 
% the seed (column) vector 'x0' for the iteration process, the tolerance 'tol' 
% which will cause the iteration to stop if the infinity-norms of successive iterates
% becomes smaller than 'tol', and 'kmax' which it the maximum number of iterations
% to perform.  
% Outputs:  the final iterate 'x', the number of iterations performed 'k', 
% and a vector 'diff' which records the 2-norms of successive differences of
% iterates.  
% If either of the last last three input variables are not specified, 
% default values of x0= zero column vector, tol=1e-10 and kmax=1000 are used.  


%assign default input variables, as necessary
if nargin<5, x0=zeros(size(b));, end
if nargin<6, tol=1e-10;, end
if nargin<7, kmax=1000;, end

if min(abs(diags(:,1)))<eps
    error('Coefficient matrix has zero diagonal entries, iteration cannot be performed.\r')
end

[n D]=size(diags);
xold=x0;
k=1;, diff=[];

while k<=kmax
    xnew=b;
    for i=1:n
        for d=2:D %run thru non-main diagonals and scan for entries that effect xnew(i)
            ind=inds(d);
            if ind<0&i>-ind %diagonal below main and j<i case
                aij=diags(i+ind,d);
                xnew(i)=xnew(i)-aij*xnew(i+ind);
            elseif ind>0&i<=n-ind %diagonal above main and j>i case
                aij=diags(i,d);
                xnew(i)=xnew(i)-aij*xold(i+ind);
            end
        end
        xnew(i)=xnew(i)/diags(i,1);
        xnew(i)=omega*xnew(i)+(1-omega)*xold(i);
    end
    diff(k)=norm(xnew-xold,inf); 
    if diff(k)<tol
            fprintf('SOR iteration has converged in %d iterations\r', k)
            x=xnew;
            return
    end
    k=k+1;, xold=xnew;
end
fprintf('SOR iteration failed to converge.\r')
x=xnew;
    
            
