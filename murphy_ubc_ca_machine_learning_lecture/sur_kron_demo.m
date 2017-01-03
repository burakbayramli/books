n = 5;
p = 2;
q = 3;
k = 3*2;
X1 = rand(n,p);
X2 = rand(n,p);
X3 = rand(n,p);
X = blkdiag(X1, X2, X3);
Sigma = rand_psd(q,q);

M = zeros(k,k);
for i=1:n
  Xi = blkdiag(X1(i,:), X2(i,:), X3(i,:));
  M = M + Xi'*Sigma*Xi;
end

M2 = X'*kron(Sigma,eye(n))*X;

  
assert(approxeq(M,M2))
