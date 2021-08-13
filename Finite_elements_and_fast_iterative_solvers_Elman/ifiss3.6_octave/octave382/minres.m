function [x,flag,relres,iter,resvec] = minres(A,b,tol,maxit,M1,M2,x0,varargin)
%minres  interfaces from matlab function to sminres  
%    [x_it,flag,relres,iter,resvec] = minres(A,b,tol,maxit,M1,M2,x0,varargin)
%   input
%          A          coefficient matrix/function handle
%          b          rhs vector
%        tol          residual reduction tolerance 
%       maxit         max number of iterations
%          M1         preconditioning matrix/function handle
%          M2         preconditioning matrix
%          x0         initial solution estimate
%    varargin         list of arguments (used for preconditioning) 
%
%   output
%        x_it         solution vector 
%        flag         termination flag (set to 0 if successful)
%      relres         final relative residual error
%        iter         actual number of iterations
%      resvec         vector of residual estimates
%   IFISS function: DJS; 26 January 2010.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 

%%%check if argument M1 is a matrix or a function
 if isa(M1,'double')
 [ x, istop, itn, rnorm, Arnorm, Anorm, Acond, ynorm, resvec] = ...
         sminres( A, b, M1*M2, 0, 0, 0, maxit, tol );
 elseif isa(M1,'function_handle')
 [ x, istop, itn, rnorm, Arnorm, Anorm, Acond, ynorm, resvec] = ...
          sminres( A, b, M1, 0, 0, 0, maxit, tol, varargin{:});
 elseif isa(M1,'char')    
 fhandle=str2func(M1);
 [ x, istop, itn, rnorm, Arnorm, Anorm, Acond, ynorm, resvec] = ...
          sminres( A, b, fhandle, 0, 0, 0, maxit, tol, varargin{:});
 else    
 error('Invalid call to preconditioned MINRES!') 
 end
%%
%% ensure compatibility with matlab function
flag=istop-1;
relres=rnorm/norm(b,2);
iter=itn-1;%itn-1;             
