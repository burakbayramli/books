function [ u_rare ] = getRarefaction( x, u_l, u_r, f_prime,f_prime_inv, T )
  u_rare = x;
  for i=1: length(x)
    if (x(i) < (f_prime(u_l)*T))
      u_rare(i) = u_l;
    elseif (x(i) < (f_prime(u_r)*T))
      u_rare(i) = f_prime_inv(x(i)/T);
    else
      u_rare(i) = u_r;
    end
  end
end
