%SQUARE_POISSONCONTROL forms and solves matrix system on square domain
%                      for Poisson control on the square domain
%   
% Within the code, the user has the following options:
%   problem - selects the toy problem 1, 2 or 3, or 'custom' (which may be
%   set up by the user using the file poissoncontrol_rhs_custom)
%   nc - grid parameter, so number of points in each dimension is 2^nc+1
%   beta - regularization parameter in the problem
%   grid_type - selects a uniform or stretched grid
%   qmethod - selects Q1 or Q2 finite element basis functions
%   system_reduce - chooses to reduce matrix to 2x2 block system, or not
%   iter_solve - solves problem directly or iteratively (by calling
%   function poissoncontrol_iter)
%   
% For toy problem 3, the user may also carry out error calculations using
% the file square_poissoncontrol_error
%   
%   IFISS function: JWP; DJS; 30 July 2012; 9 January 2019
% Copyright (c) 2012 J.W. Pearson 

% Make choices as to the problem being solved
problem = default('problem number 1/2/3? (default is 1)',1);
nc = default('grid parameter: 3 for underlying 8x8 grid (default is 16x16)',4);
if nc<2, error('illegal parameter choice ''nc'', try again.'), end
beta = default('regularization parameter beta (default is 0.01)',0.01);
if beta<eps, error('illegal parameter choice ''beta'', try again.'), end
if problem==2,
grid_type = default('uniform/stretched grid (1/2) (default is uniform)',1);
else, grid_type=1; end % uniform grid
square_type = 2;
qmethod = default('Q1/Q2 approximation 1/2? (default Q1)',1);

% How large is resulting stiffness or mass matrix?
np = (2^nc+1)^2; % entire matrix system is thus 3*np-by-3*np in size

% Compute matrices specifying location of nodes
[x,y,xy,bound,mv,mbound] = square_domain_x(nc,grid_type);

% Compute connectivity, stiffness and mass matrices, for Q1 or Q2 elements
if qmethod==1
    [ev,ebound] = q1grid(xy,mv,bound,mbound);
    [K,M] = femq1_diff(xy,ev);
elseif qmethod==2
    [x,y,xy] = q2grid(x,y,xy,mv,bound); % to generate plot of Q2 elements
    [K,M] = femq2_diff(xy,mv);
else
    error('illegal parameter choice ''qmethod'', try again.')
end

% Specify vectors relating to desired state and Dirichlet BCs
if problem==1
    [yhat_vec,bc_nodes] = poissoncontrol_rhs_ex1(xy,bound,square_type);
elseif problem==2
    [yhat_vec,bc_nodes] = poissoncontrol_rhs_ex2(xy,bound,square_type);
elseif problem==3
    [yhat_vec,bc_nodes] = poissoncontrol_rhs_ex3(xy,bound);
elseif strcmp(problem,'custom')==1
    [yhat_vec,bc_nodes] = ...
        poissoncontrol_rhs_custom(xy,bound,square_type);
else
    error('illegal parameter choice ''problem'', try again.')
end

% Initialise RHS vector corresponding to desired state
Myhat = M*yhat_vec;

% Enforce Dirichlet BCs on state, and zero Dirichlet BCs on adjoint
[K,d] = nonzerobc_input(K,zeros(np,1),xy,bound,bc_nodes);
[M,Myhat] = nonzerobc_input(M,Myhat,xy,bound,bc_nodes);

% Choose to reduce the size of the matrix system, and whether to solve it
% directly or iteratively
system_reduce = default('reduce matrix system to 2x2 block (1) or not (0)? (default is 0)',0);
iter_solve = default('direct (0) or iterative (1) solution? (default 0)',0);

if iter_solve==0
    if system_reduce==0 % build and solve 3x3 block matrix system
        Matrix = [M sparse(np,np) K; ...
            sparse(np,np) beta*M -M; ...
            K -M sparse(np,np)];
        rhs = [Myhat; zeros(np,1); d];

        tic, fprintf('solving linear system ...  ')
        x_it = Matrix\rhs;
        fprintf('done\n')
        etoc=toc; fprintf('Galerkin system solved in  %8.3e seconds\n\n',etoc)

        sol_y = x_it(1:np); sol_u = x_it(np+1:2*np);
    elseif system_reduce==1 % build and solve 2x2 block matrix system
        Matrix = [M K; ...
            K -1/beta*M];
        rhs = [Myhat; d];

        tic, fprintf('solving linear system ...  ')
        x_it = Matrix\rhs;
        fprintf('done\n')
        etoc=toc; fprintf('Galerkin system solved in  %8.3e seconds\n\n',etoc)

        sol_y = x_it(1:np); sol_u = 1/beta*x_it(np+1:end);
    else
        error('illegal parameter choice ''system_reduce'', try again.')
    end
elseif iter_solve==1 % go to iterative method suite to solve system
    if system_reduce==0
        it_solve_poissoncontrol
        
        sol_y = x_it(1:np); sol_u = x_it(np+1:2*np);
    elseif system_reduce==1
        it_solve_poissoncontrol_reduced
        
        sol_y = x_it(1:np); sol_u = 1/beta*x_it(np+1:end);
    end
end

% Make contour and surface plots of solutions for state y and control u
solplot_poissoncontrol(sol_y,sol_u,xy,x,y,77)
    
% Carry out error estimates for Problem 3
if problem==3
error_test = default('carry out error test (1) or not (0)? (default is 0)',0);
if error_test==1, poissoncontrol_errortest(sol_y,sol_u,xy,beta); end
end
