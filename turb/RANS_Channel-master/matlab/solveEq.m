%**************************************************************************
%       function to solve linear system  [A].x = b
%**************************************************************************
% Inputs:
%   x       variable to be solved
%   A       coefficient matrix
%   b       right hand side
%   omega   under-relaxation parameter
%
% Output:
%   x_new   updated x
%
function [x_new] = solveEq(xfull,Afull,b,omega)

    x_new = xfull;
    
    % add boundary conditions
    b = b - xfull(1)*Afull(2:end-1,1) - xfull(end)*Afull(2:end-1,end);
    
    % perform under-relaxation
    A = Afull(2:end-1,2:end-1);
    b = b + (1-omega)/omega * diag(A).*xfull(2:end-1);
    A(logical(eye(size(A))))=diag(A)/omega;
    
    % solve linear system
    x_new(2:end-1) = A\b;
    
end

