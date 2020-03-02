A = [1 4 -2 -1 0; 1 3 2 -1 0; 2 1 2 3 -1]; % create the matrix A
b = [8; 10; 20]; % create the vector b
Aeq = [1 3 -4 -1 1]; %create the matrix Aeq
beq = [7]; % create the vector beq
lb = [5 1 0 0 0]; % create the vector lb
ub = [Inf Inf Inf Inf Inf]; % create the vector ub
c = [-2; -3; 1; 4; 1]; %create the vector c
% call the linprog solver
[x, objVal] = linprog(c, A, b, Aeq, beq, lb, ub);
for i = 1:5 % print results in formatted form
	fprintf('x%d \t %20.4f\n', i, x(i))
end
fprintf(['The value of the objective function ' ...
    'is %.4f\n'], objVal)