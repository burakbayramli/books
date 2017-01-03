function [w,Smat,alpha,loglik]=BayesLogRegression(phi,c,w,alpha,R,opts)
%[w,Smat,alpha,loglik]=BayesLogRegression(phi,c,w,alpha,R,opts)
% Bayesian Logistic Regression
%
% Inputs:
% phi : M*N matrix of phi vectors on the N training points
% c : N*1 vector of associated class lables (0,1)
% w : initial weight vector
% alpha : initial regularisation parameter
% opts.HypUpdate : 1 for EM, 2 for Gull-MacKay
% opts.HypIterations : number of hyper parameter updates
% opts.NewtonIterations : number of Newton Updates
%
% Outputs:
% w : learned posterior mean weight vector
% Smat : posterior covariance
% alpha : learned regularisation parameter
% loglik : log likelihood of training data for optimal parameters
s=2*c(:)-1; [M N]=size(phi); logdetR = logdet(R); alphas=[];
for alphaloop=1:opts.HypIterations
    for wloop=1:opts.NewtonIterations % Newton update for Laplace approximation
        sigmawh = sigma(s.*(phi'*w));
        gE=alpha.*R*w; J=zeros(M);
        for n=1:N
            gE = gE-(1-sigmawh(n))*phi(:,n).*s(n);
            J = J + sigmawh(n)*(1-sigmawh(n))*phi(:,n)*phi(:,n)';
        end
        Hess= alpha*R+ J;
        w = w-0.5*(Hess\gE);
    end
    Smat = inv(Hess);
    L(alphaloop)=-0.5*alpha*w'*R*w+sum(logsigma(s.*(phi'*w)))-0.5*logdet(Hess)+M*0.5*log(alpha)+0.5*logdetR;
    switch opts.HypUpdate
        case 1
            alpha = M./(w'*R*w+trace(R*Smat)); % EM update
        case 2
            alpha = min(10000,(M-alpha.*trace(R*Smat))./(w'*R*w)); % MacKay/Gull update
    end
    alphas=[alphas alpha];
    if opts.plotprogess
        subplot(1,3,1); plot(L); title('likelihood');
        subplot(1,3,2); plot(log(alphas)); title('log alpha');
        subplot(1,3,3); bar(w); title('mean weights');drawnow
    end
end
loglik=L(end);