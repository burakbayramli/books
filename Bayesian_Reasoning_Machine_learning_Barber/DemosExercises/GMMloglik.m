function logl=GMMloglik(X,P,m,S)
%logl=GMMloglik(X,P,m,S)
% Log Likelihood of data X under a Gaussian Mixture Model
N=size(X,2); H=size(m,2);
for n = 1:N
	for i = 1:H
		invSi = inv(S(:,:,i));
		logdetSi=logdet(2*pi*S(:,:,i));
		v = X(:,n) - m(:,i);
		logp(n,i) =-0.5*v'*invSi*v - 0.5*logdetSi + log(P(i));
	end
	logl(n)=  logsumexp(logp(n,:),ones(1,H));
end