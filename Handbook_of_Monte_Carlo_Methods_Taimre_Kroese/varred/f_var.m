function out = f_var(nu,U,N)
nu1 = repmat(nu,N,1);
W = prod(1./(nu1.*U.^(nu1 - 1)),2);
y = h(U);
out = W'*y.^2;
