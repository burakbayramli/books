function [m S alpha beta marglik]=BayesLinReg(y,sumPhi,sumyPhi,opts)
%BAYESLINREG Bayesian Linear Regression training using basis functions phi(x)
% [m S alpha beta marglik]=BayesLinReg(y,sumPhi,sumyPhi,opts)
%
% Inputs:
% x : training inputs
% y : training outputs
% sumPhi : sum_n phi(x^n)phi(x^n)'
% sumyPhi : sum_n y^n phi(x^n)
% opts.maxits
% opts.tol
% opts.plotprogress
%
% Outputs:
% m : posterior mean weight
% S : posterior covariance matrix
% alpha : learned weight hyperparameter
% beta : learned noise hyperparamter 
% marglik : model likelihood p(data|optimal hyperparameters)
B=length(sumyPhi); N=length(y);
sumyy=sum(y.^2);
beta=1; alpha=1; % initial guess for the hyperparameters
for loop=1:opts.maxits
	invS=alpha*eye(B)+beta*sumPhi;
	S=inv(invS);
	m=beta*S*sumyPhi;
	beta=N./(sumyy-2*m'*sumyPhi+m'*sumPhi*m+trace(S*sumPhi));
	alpha=B/(trace(S)+m'*m);
	d=beta*sumyPhi;
	logpDgG(loop)=-beta*sumyy+d'*invS*d-log(det(S))+B*log(alpha)+N*log(beta)-N*log(2*pi);
	if loop>1
		if logpDgG(loop)-logpDgG(loop-1)<opts.tol; break; end
	end
	if opts.plotprogress; plot(logpDgG); drawnow;end
end
marglik=logpDgG(end);