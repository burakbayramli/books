function result = e_approx(n)
%  e_approx(n)
%
% Computes an approximation of the natual number e using a series.

  result = 1;
  for(i = 1:n)
    result = result + 1 / fact(i);
  end
