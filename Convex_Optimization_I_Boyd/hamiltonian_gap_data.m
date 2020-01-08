% data for hamiltonian perturbation optimization
% ee364a, s. boyd
randn('state',0);
n = 10;
k = 15;
Hnom = randn(n);
Hnom = Hnom+Hnom';
Hpert = cell(k,1);
for i=1:k
Hpert{i} = randn(n);
Hpert{i} = (Hpert{i}+Hpert{i}');
end
