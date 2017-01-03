function [w,Smat,alpha,loglik]=BayesLogRegressionRVM(phi,c,w,alpha,opts)
%BAYESLOGREGRESSIONRVM Bayesian Logistic Regression with the Relevance Vector Machine
%[w,Smat,alpha,loglik]=BayesLogRegressionRVM(phi,c,w,alpha,opts)
% Bayesian Logistic Regression
%
% Inputs:
% phi : M*N matrix of phi vectors on the N training points
% c : N*1 vector of associated class lables (0,1)
% w : initial weight vector
% alpha : initial regularisation vector
% opts.HypUpdate : 1 for EM, 2 for Gull-MacKay
% opts.HypIterations : number of hyper parameter updates
% opts.NewtonIterations : number of Newton Updates
%
% Outputs:
% w : learned posterior mean weight vector
% Smat : posterior covariance
% alpha : learned regularisation vector
% loglik : log likelihood of training data for optimal parameters
s=2*c(:)-1; [M N]=size(phi);
for alphaloop=1:opts.HypIterations
	for wloop=1:opts.NewtonIterations % Newton update for Laplace approximation
		sigmawh = sigma(s.*(phi'*w));
		gE=alpha.*w; J=zeros(M);
		tmp=zeros(N,1);
		for n=1:N
			gE = gE-(1-sigmawh(n))*phi(:,n).*s(n);
			J = J + sigmawh(n)*(1-sigmawh(n))*phi(:,n)*phi(:,n)';
		end
		Hess= diag(alpha)+ J;
		w = w-inv(Hess)*gE;
	end
	Smat = inv(Hess);
	L(alphaloop)=-0.5*w'*diag(alpha)*w+sum(log(sigma(s.*(phi'*w))))-0.5*logdet(Hess)+0.5*sum(log(alpha));
	switch opts.HypUpdate
		case 1
			alpha = 1./(w.*w+diag(Smat)); % EM update
		case 2
			alpha = min(10000,(ones(M,1)-alpha.*diag(Smat))./(w.*w)); % MacKay/Gull update
	end
	if opts.plotprogess
		subplot(1,3,1); plot(L); title('likelihood');
		subplot(1,3,2); plot(log(alpha)); title('log alpha');
		subplot(1,3,3); bar(w); title('mean weights');drawnow
	end
end
loglik=L(end);