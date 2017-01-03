function s = signperm(p)
%SIGNPERM Sign of a permutation.
n = length(p);
s = 1;
for j = 1:n
   k = find(p == j);
   if k ~= j
      p([k,j]) = p([j,k]);
      s = -s;
   end
end
