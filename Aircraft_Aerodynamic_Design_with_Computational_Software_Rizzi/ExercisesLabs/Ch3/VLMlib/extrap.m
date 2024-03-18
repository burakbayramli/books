function yext = extrap(y,order,q)
  % Richardson extrap order 
  % ratio between stepsizes q
  n = length(y);
  for k = 1:n-1
    f = q^(order*k)-1;
    for j = n:-1:k+1
      tmp = y{j};
      tmp0 = y{j-1};
      y{j} = tmp + 1/f*(tmp-tmp0);
%      for m = 1:n
%        tmp = y{m};
%        fprintf('%f ',tmp)
%      end
%      fprintf('\n')
    end
%    fprintf('\n')
  end
  yext = y{n};

