function s = permdet(p)
%PERMDET Determinant of a permutation.
n = length(p);
I = eye(n);
s = determ(I(p,:));
