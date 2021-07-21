function s = simplesum(n)
% simplesum(n)
%
% Computes the sum of 1 + 2 + ... + n.

  s = 0;
  for(i = 1:n)
    s = s + i;
  end
