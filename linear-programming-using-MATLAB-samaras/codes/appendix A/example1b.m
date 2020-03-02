A = [-2 0 -3 0; -3 2 0 -4]; % create the matrix A
b = [-6; -8]; % create the vector b
Aeq = [4 -3 8 -1; 4 0 -1 4]; %create the matrix Aeq
beq = [20; 18]; % create the vector beq
lb = [1 0 2 0]; % create the vector lb
ub = [Inf Inf 10 Inf]; % create the vector ub
c = [-2; 4; -2; 2]; %create the vector c
% call the linprog solver
[x, objVal] = linprog(c, A, b, Aeq, beq, lb, ub);
for i = 1:4 % print results in formatted form
	fprintf('x%d \t %20.4f\n', i, x(i))
end
fprintf(['The value of the objective function ' ...
    'is %.4f\n'], objVal)