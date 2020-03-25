%wtgrad.m
N = 10^5; 
K = 40;
for i=1:K
    mu(i) = 1.5 + (i-1)*0.1;
end
mutil = 2; %simulate under this
lam = 1;
Rmat = zeros(N,K);
taumat = zeros(N,K);
gradWmat = zeros(N,K);
gradWmatHmat = zeros(N,K);
W = zeros(1,K);
Scor = zeros(1,K);
tau = zeros(1,K);
R = zeros(1,K);
for i = 1:N
    B = -log(rand)/mutil;
    W = mu.*exp(-mu.*B)/(mutil*exp(-mutil*B));
    A = -log(rand)/lam;
    H = max( B - A, 0);
    Scor = 1./mu - B;
    tau = W; %sum of W's
    R = H*W; %sum of H*W's
    gradW = Scor.*W; %sum of W*S's
    gradWH = Scor.*W*H; %sum of W*S*H
    while (H > 0)
        B = -log(rand)/mutil;
        W = W.*mu.*exp(-mu.*B)/(mutil*exp(-mutil*B));
        A = -log(rand)/lam;
        H = max( H +  B - A, 0);
        Scor = Scor + 1./mu - B;
        tau = tau+W;
        R = R + H*W;
        gradW = gradW + Scor.*W; %sum of W*S
        gradWH = gradWH + Scor.*W*H; %sum of W*S*H
    end
    taumat(i,:) = tau;
    Rmat(i,:) = R;
    gradWmat(i,:) = gradW;
    gradWmatHmat(i,:) = gradWH;
end
ell = zeros(1,K);
eltrue = lam.*(lam - 2*mu)./(lam - mu).^2./mu.^2;
for k=1:K
    ell(k) = mean(gradWmatHmat(:,k))/mean(taumat(:,k)) ...
           -(mean(Rmat(:,k))/mean(taumat(:,k))) ...
	   *(mean(gradWmat(:,k))/mean(taumat(:,k)));
end
clf
hold on
plot(mu,ell,'.');
plot(mu,eltrue,'r');
hold off
