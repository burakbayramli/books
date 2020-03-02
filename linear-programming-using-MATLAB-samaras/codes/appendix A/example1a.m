variables = {'x1', 'x2', 'x3', 'x4'}; % construct a cell
% array with the names of the variables
n = length(variables); % find the number of the variables
for i = 1:n % create x for indexing
	eval([variables{i}, ' = ', num2str(i), ';']);
end
lb = zeros(1, n); % create a vector and add a lower bound
% of zero to all variables
lb([x1, x3]) = [1, 2]; % add lower bounds to variables x1,
% x3
ub = Inf(1, n); % create a vector and add an upper bound of
% Inf to all variables
ub(x3) = 10; % add an upper bound to variable x3
A = zeros(2, n); % create the matrix A
b = zeros(2, 1); % create the vector b
% declare the 1st constraint
A(1, [x1, x3]) = [-2, -3]; b(1) = -6;
% declare the 2nd constraint
A(2, [x1, x2, x4]) = [-3, 2, -4]; b(2) = -8; 
Aeq = zeros(2, n); % create the matrix Aeq
beq = zeros(2, 1); % create the vector beq
% declare the 1st equality constraint
Aeq(1, [x1, x2, x3, x4]) = [4, -3, 8, -1]; beq(1) = 20;
% declare the 2nd equality constraint
Aeq(2, [x1, x3, x4]) = [4, -1, 4]; beq(2) = 18;
c = zeros(n, 1); % create the objective function vector c
c([x1 x2 x3 x4]) = [-2; 4; -2; 2];
% call the linprog solver
[x, objVal] = linprog(c, A, b, Aeq, beq, lb, ub);
for i = 1:n % print results in formatted form
    fprintf('%s \t %20.4f\n', variables{i}, x(i))
end
fprintf(['The value of the objective function ' ...
    'is %.4f\n'], objVal)