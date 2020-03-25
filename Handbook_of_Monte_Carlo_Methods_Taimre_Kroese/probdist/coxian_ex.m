%coxian_ex.m
alpha = [1 0 0 0];
p = 0.9;
m = 4;
lam = 3;
A = [-lam , lam*p, 0, 0; 0, -lam, lam*p ,0 ; ...
    0 ,0, -lam, lam*p; 0, 0, 0 ,-lam] ;
A1 = - A*ones(m,1); 
q = - diag(A); 
Q = [A A1; zeros(1,m) 0]; 
K = diag([1./q; 1])*(Q + diag([q;0]));
K(m+1,m+1) = 1;
N = 10^5;
x = zeros(N,1); 
for i=1:N;
    T = 0;
    Y = min(find(cumsum(alpha)> rand));
    while Y ~= m+1
        T= T -log(rand)/q(Y);
        Y = min(find(cumsum(K(Y,:))> rand));
    end
    x(i) = T;
end

truemean = -alpha*inv(A)*ones(m,1)
mx = mean(x)
truevar = alpha*inv(A)*(2*eye(m,m)-ones(m,1)*alpha)*inv(A)*ones(m,1)
vx = var(x)
hist(x,100)
