function lee_HW2_P3
% Initialize input guesses
x1i=[1.5;1.5;2;2];
x2i=[-1.5;2.1;-1.9;2.1;-1.9;2.1;-1.5;2.1];

% Set tolerances
atol = 1e-8;

% Run newton method
[x1,table1,steps,evals]=newton_solve(x1i,atol);
[x2,table2]=newton_solve(x2i,atol);

% Output converged solution
% Answer should yield a vector of ones in both cases
fprintf('Checks\n')
x1
x2

% Make subplots showing newton convergence
Iter=0:1:5;
figure(1)
for i=1:4
    subplot(2,2,i)
    plot(Iter,steps(i,:),'o-')
    xlabel('Iteration Number')
    ylabel(['x_',num2str(i)])
end

figure(2)
for i=1:4
    subplot(2,2,i)
    plot(Iter,evals(i,:),'*-')
    xlabel('Iteration Number')
    ylabel(['F_',num2str(i)])
end

% Check spartsity 
[F,J] = FunctionEvaluator(x1i);
figure(3)
spy(J)

[F,J] = FunctionEvaluator([x2i;x2i]);
figure(4)
spy(J)

% Run method with sparse representation
% Repeat 5 times to get average CPU time
t_full = 0;
t_sparse = 0;
N_repeat = 5; % number of repetitions

for i=1:N_repeat
    % Do full calculation
    ts1=tic;
    [x1]=newton_solve(x1i,atol);
    t1 = toc(ts1);
    t_full = t_full + t1;
     
    % Repeat sparse calculation
    ts2 = tic;
    [x1_sparse]=newton_solve_sparse(x1i,atol);
    t2 = toc(ts2);
    t_sparse = t_sparse + t2;
end

fprintf('Full Jacobian time = %6.4e\n',t_full/N_repeat)
fprintf('Sparse Jacobian time = %6.4e\n',t_sparse/N_repeat)

% Check that sparse Jacobian yields the same solution
fprintf('Check Sparse Jacobian\n')
x1_sparse

end

function [x_out,table,steps,evals]=newton_solve(x0,atol)
%function [x_out]=newton_solve(x0,atol)
% Function to solve nonlinear equations iteratively using Newton's method
% and an analytically determined Jacobian matrix
% Inputs:
%           x0 = initial guess for the solution routine
%           atol = absolute tolerance of the solver
% Outputs:
%           x_out = final value of x satisfying max(|func(x)|) < abs tol
%           table = a table listing num_iter, norm(f), and norm(x)
%           steps = value of x per iteration
%           eval = function value per iteration

% Set tolerances
tols = atol; 
index = 0;

% We set a maximum number of iterations in order to ensure that the Newton
% solver does not loop indefinitely.
max_iter = 200;

% Take the initial guess
x = x0;
         
while (0==0)
    if (index > max_iter)
           fprintf('Error using the Newton solver. Max iterations reached.\n');
           x_out = x;
           break;
    end
    
    % Evaluate function and the Jacobian
    [f,J]=FunctionEvaluator(x);
    
    % Convergence criterion; function norm
    if (norm(f) <tols)
        x_out = x;
        break
    else
        % Output Newton's method algorithm results if outputs are called
        if nargout>1
           if index == 0
               fprintf('%7s %7s %7s\n','Iter','norm(f)','norm(x)')
           end
           norm_f = norm(f);
           norm_x = norm(x);
           fprintf('%7i %7.3f %7.3f\n',index,norm_f,norm_x)
           table(index+1,:)=[index,norm_f,norm_x];
        end
        if nargout>2 && index<6
           steps(:,index+1)=x;
           evals(:,index+1)=f;
        end
        
        % Update Newton's step
        delx = -J\f;
        x=x+delx; 
        index = index + 1;  


    end

end


end

function [x_out]=newton_solve_sparse(x0,atol)
% Works the same as newton_solve, with only minor changes as noted.
% Inputs:
%           x0 = initial guess for the solution routine
%           atol = absolute tolerance of the solver
% Outputs:
%           x_out = final value of x satisfying max(|func(x)|) < abs tol

% Set tolerances
tols = atol; 

% We set a maximum number of iterations in order to ensure that the Newton
% solver does not loop indefinitely.
max_iter = 200;
index = 0;

% Take the initial guess
x = x0;

% Define odd and even indices
n=length(x0);
odds=[1:2:n];
evens=[2:2:n];

% Initialize Newton's step vector
delx=zeros(n,1);

while (0==0)
    if (index > max_iter)
           fprintf('Error using the Newton solver. Max iterations reached.\n');
           x_out = x;
           break;
    end
    
    % Evaluate function and the Jacobian
    [f,J]=SparseEvaluator(x);
    
    % Convergence criterion; function norm
    if (norm(f) <tols)
        x_out = x;
        break
    else
        % Calculate Newton's step using the sparsity pattern of the Jacobian
        delx(odds)=-f(odds)./J(:,1);
        delx(evens)=(-f(evens)-delx(odds).*J(:,3))./J(:,2);

        % Update Newton's step
        x=x+delx; 
        index = index + 1;     
    end

end



end

function [F,J] = FunctionEvaluator(x)
% Evaluate the vector function and the Jacobian matrix for 
% the system of nonlinear equations derived from the general 
% n-dimensional Rosenbrock function.
% Get the problem size
n = length(x);  
if n == 0, error('Input vector, x, is empty.'); end
if mod(n,2) ~= 0, 
   error('Input vector, x ,must have an even number of components.');
end
% Evaluate the vector function
odds  = 1:2:n;
evens = 2:2:n;
F = zeros(n,1);
C = zeros(n,n); D = zeros(n,n); E=zeros(n,n);
F(odds,1)  = (1-x(odds)).^2;
F(evens,1) = 10.*((x(odds)-x(evens).^2)).^2; 
% Evaluate the Jacobian matrix if nargout > 1 
if nargout > 1
   c = 2.*(x(odds)-1);    C(sub2ind([n,n],odds,odds))=c;
   d = 40.*(x(evens).*(x(evens).^2-x(odds)));  D(sub2ind([n,n],evens,evens))=d;
   e = 20.*(x(odds)-x(evens).^2);    E(sub2ind([n,n],evens,odds))=e;
   J = C + D + E;
end
end

function [F,J] = SparseEvaluator(x)
% Evaluate the vector function and the Jacobian matrix for 
% the system of nonlinear equations derived from the general 
% n-dimensional Rosenbrock function.
% Get the problem size
n = length(x);  
if n == 0, error('Input vector, x, is empty.'); end
if mod(n,2) ~= 0, 
   error('Input vector, x ,must have an even number of components.');
end
% Evaluate the vector function
odds  = 1:2:n;
evens = 2:2:n;
F = zeros(n,1);
%C = zeros(n,n); D = zeros(n,n); E=zeros(n,n);
F(odds,1)  = (1-x(odds)).^2;
F(evens,1) = 10.*((x(odds)-x(evens).^2)).^2; 

% Evaluate the Jacobian matrix if nargout > 1 
% Stores Jacobian as a 3xN/2 matrix with following values
% Column 1: Diagonal values, odd rows
% Column 2: Diagonal values, even rows
% Column 3: Off-diagonal elements, even rows.
if nargout > 1
  J=zeros(n/2,3);
  J(:,1)=-2*(1-x(odds));
  J(:,2)=-40*x(evens).*(x(odds)-x(evens).^2);
  J(:,3)=20*(x(odds)-x(evens).^2);
end
end