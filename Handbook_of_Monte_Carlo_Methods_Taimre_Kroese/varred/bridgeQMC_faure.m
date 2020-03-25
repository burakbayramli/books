%bridgeQMC_faure.m
K = 20;
N = 10^4/K;
F = faure(5,5,N-1);
for i=1:K
    U(:,:,i) = mod(F + repmat(rand(1,5),N,1), 1);
end
for i=1:K
    y(i) = mean(h(U(1:N,:,i)));
end
ell = mean(y);          %estimate
se = std(y)/sqrt(K); %standard error
fprintf('ell=%g, percRE = %g  \n',ell, 100*se/ell);
