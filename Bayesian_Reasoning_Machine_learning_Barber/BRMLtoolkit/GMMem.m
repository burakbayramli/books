function [P,m,S,loglik,phgn]=GMMem(X,H,opts)
%GMMEM Fit a mixture of Gaussian to the data X using EM
% [P,m,S,loglik,phgn]=GMMem(X,H,opts)
%
% X : each column of X is a datapoint.
% H : number of components of the mixture.
% opts.plotprogress=1 to display the likelihood during training
% opts.maxit : number of EM iterations
% if opts.pars is empty a random parameter initialisation is used,
% otherwise opts.pars.P contains the mixture probabilities, opts.pars.m the
% mixture means and opts.pars.S the covariance matrices
%
% Outputs:
% P : learned mixture coefficients
% m :learned means
% S : learned covariances
% loglik is the log likelihood
% phgn are the mixture assignment probabilties
% See also demoGMMem.m
D = size(X,1); % dimension of the space
N = size(X,2); % number of training patterns

if isfield(opts,'pars')
	P=opts.pars.P; m=opts.pars.m; S=opts.pars.S;
else
	r = randperm(N); m = X(:,r(1:H)); % initialise the centres to random datapoints
	s2 = mean(diag(cov(X')));
	S = repmat(s2*eye(D),[1 1 H]); % initialise the variances to be large
	P = ones(H,1)./H;  % intialise the component probilities to be uniform
end
for emloop = 1:opts.maxit
	% E-step:
	for i = 1:H
		invSi = inv(S(:,:,i));
		logdetSi=logdet(2*pi*S(:,:,i));
		for n = 1:N
			v = X(:,n) - m(:,i);
			logpold(n,i) =-0.5*v'*invSi*v - 0.5*logdetSi + log(P(i));
		end
	end
	phgn=condexp(logpold'); % responsibilities
	pngh = condp(phgn'); % membership

	logl(emloop)=0;
	for n=1:N
		logl(emloop)= logl(emloop) + logsumexp(logpold(n,:),ones(1,H));
	end
	if opts.plotlik; subplot(1,2,2); plot(logl); title('log likelihood');drawnow; end
	if opts.plotsolution
		subplot(1,2,1); cla;
		if D==2;% if data is 2 dimensional
			plot(X(1,:),X(2,:),'o'); hold on;
			for i=1:H
				x=pointsCov(m(:,i),S(:,:,i)); plot(x(1,:),x(2,:),'r-','linewidth',2)
			end; drawnow;
		else
			bar(P); title('mixture probabilities')
		end
	end

	% M-step:
	for i = 1:H % now get the new parameters for each component
		tmp = (X - repmat(m(:,i),1,N)).*repmat(sqrt(pngh(:,i)'),D,1);
		Scand =  tmp*tmp';
		if det(Scand)> opts.minDeterminant % don't accept too low determinant
			S(:,:,i) = Scand;
		end
	end
	m = X*pngh;
	P = sum(phgn,2)/N;
end
loglik=logl(end);