function rho = kapfun(xy)
n = size(xy,1);
rho = zeros(n,1);
for k = 2:n-1
   A = [2*xy(k-1:k+1,1),2*xy(k-1:k+1,2),ones(3,1)];
   tmp = A\sum(xy(k-1:k+1,:).^2,2);
   rho(k) = sqrt(tmp(3)+tmp(1)^2+tmp(2)^2);
end
rho(1)=rho(2);
rho(n)=rho(n-1);