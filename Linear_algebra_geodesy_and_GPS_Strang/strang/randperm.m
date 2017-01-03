function p = randperm(n)
%RANDPERM Random permutation.
%	RANDPERM(n) is a random permutation of 1:n.
[ignore,p] = sort(rand(1,n));
