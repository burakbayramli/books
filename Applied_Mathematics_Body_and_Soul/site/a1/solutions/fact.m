function result = fact(n)
% fact(n)
%
% The factorial function:
% fact(0) = 1
% fact(n) = 1 * 2 * ... * n

  if(n == 0)
    result = 1;
  else
    result = 1;
    for(i = 1:n)
      result = result * i;
    end
  end
