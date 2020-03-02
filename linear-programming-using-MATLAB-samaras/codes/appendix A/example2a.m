variables = {'x1', 'x2', 'x3', 'x4', 'x5'}; % construct a
% cell array with the names of the variables
n = length(variables); % find the number of the variables
for i = 1:n % create x for indexing
    eval([variables{i}, ' = ', num2str(i), ';']);
end
lb = zeros(1, n); % create a vector and add a lower bound
% of zero to all variables
lb([x1, x2]) = [5, 1]; % add lower bounds to variables x1,
% x2
ub = Inf(1, n); % create a vector and add an upper bound
% of Inf to all variables
A = zeros(3, n); % create the matrix A
b = zeros(3, 1); % create the vector b
% declare the 1st inequality constraint
A(1, [x1, x2, x3, x4]) = [1, 4, -2, -1]; b(1) = 8;
% declare the 2nd inequality constraint
A(2, [x1, x2, x3, x4]) = [1, 3, 2, -1]; b(2) = 10;
% declare the 3rd inequality constraint
A(3, [x1, x2, x3, x4, x5]) = [2, 1, 2, 3, -1]; b(3) = 20;
Aeq = zeros(1, n); % create the matrix Aeq
beq = zeros(1, 1); % create the vector beq
% declare the equality constraint
Aeq(1, [x1, x2, x3, x4, x5]) = [1, 3, -4, -1, 1]; beq(1) = 7;
c = zeros(n, 1); % create the objective function vector c
c([x1 x2 x3 x4 x5]) = [-2; -3; 1; 4; 1];
% call the linprog solver
[x, objVal] = linprog(c, A, b, Aeq, beq, lb, ub);
for i = 1:n % print results in formatted form
	fprintf('%s \t %20.4f\n', variables{i}, x(i))
end
fprintf(['The value of the objective function ' ...
    'is %.4f\n'], objVal)