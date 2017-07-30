function [ x, err, iter, flag ] = sor(A, x, b, w, max_it, tol)
%
% [ x, err, iter, flag ] = sor(A, x, b, w, max_it, tol)
%
% SOR Successive Over-Relaxation Method
%   This function solves linear equation systems such as Ax=b using SOR 
%   method (Successive Over-Relaxation).
%   When the relaxation scalar w=1, the method used is Gauss-Seidel.
%
% Input:
%   A - input matrix
%   x - initial vector
%   b - vector b
%   w - relaxation scalar
%   max_it - maximum number of iterations
%   tol - tolerance
%
% Output:
%   x - solution vector
%   err - norm err estimate
%   iter - nu,ber of performed iterations
%   flag - 0 = a solution was found / 1 = no convergence for max_it
%
% Example:
%   [ x, err, iter, flag ] = sor( [.5 .125; .125 .25], zeros(2,1),
%       ones(2,1), .5, 1e4, 1e-4 )
%
% Author:	Tashi Ravach
% Version:	1.0
% Date:     08/06/2006
%

    if nargin == 3
        w = .5;
        max_it = 1e4;
        tol = 1e-4;
    elseif nargin == 4
        max_it = 1e4;
        tol = 1e-4;
    elseif nargin == 5
        tol = 1e-4;
    elseif nargin ~= 6
        error('sor: invalid input parameters');
    end
    
    flag = 0;
    iter = 0;

    norma2_b = norm(b);
    if (norma2_b == 0.0)
        norma2_b = 1.0;
    end

    r = b - A * x;
    err = norm(r) / norma2_b;
    if (err < tol)
        return
    end

    % separate A into several matrix for SOR/Gauss-Seidel
    [ M, N, b ] = matsep(A, b, w, 2);

    for iter = 1 : max_it
        x_1 = x;
        x = M \ (N * x + b); % adjust the aproximation
        %err = norm(x - x_1) / norm(x); % compute error
        err = norm(x_1 - x, 1); % compute error
        if (err <= tol) % check for convergence
            break
        end          
    end
    b = b / w; % vector b

    if (err > tol) % no convergence
        flag = 1;
    end   
    
end


function [ M, N, b ] = matsep(A, b, w, flag)
%
% [ M, N, b ] = matsep(A, b, w, flag)
%
% MATSEP Matrix Separation
%   Input matrix is splitted into several others in diferent ways depending
%   on the method to be used: Jacobi and SOR (Gauss-Seidel when w = 1)
%
% Input:
%   A - input matrix
%   x - inicial vector
%   b - vector b
%   flag - 1 = Jacobi / 2 = SOR
%
% Output:
%   M - matrix M
%   N - matrix N
%   b - vector b (modified for SOR)
%
% Author:	Tashi Ravach
% Version:	1.0
% Date:     08/06/2006
%

    if nargin ~= 4
        error('matsep: invalid input parameters');
    end
    
    [ m, n ] = size(A);
    if m ~= n
        error('matsep: input matrix A must have dimension nXn');
    end
    
    [ l, o ] = size(b);
    if l ~= n && o ~= 1
        error('matsep: input matrix b must have dimension nX1');
    end

    if (flag == 1) % separation for Jacobi
        M = diag(diag(A));
        N = diag(diag(A)) - A;
    elseif (flag == 2) % separation for SOR/Gauss-Seidel
        b = w * b;
        M =  w * tril(A, -1) + diag(diag(A));
        N = -w * triu(A,  1) + (1.0 - w) * diag(diag(A));
    end

end
