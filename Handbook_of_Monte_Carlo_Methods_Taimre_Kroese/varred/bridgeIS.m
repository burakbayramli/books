%bridgeIS.m
N = 10^4;
nu0 = [1.3, 1.1, 1, 1.3, 1.1];
nu = repmat(nu0,N,1);
U = rand(N,5).^(1./nu);
W = prod(1./(nu.*U.^(nu - 1)),2);
y = h(U).*W;
est = mean(y)
percRE = std(y)/sqrt(N)/est*100
