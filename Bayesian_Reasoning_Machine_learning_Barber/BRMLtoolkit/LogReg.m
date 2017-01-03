function [w b p10 p11 ul l]=LogReg(x0,x1,varargin)
%LOGREG  Learning Logistic Linear Regression Using Gradient Ascent (BATCH VERSION)
% [w b p10 p11]=LogReg(x0,x1,<opts>)
%
% maximise sum_n (2*c(n)-1)log sigma(w'*x{n}+b)+lambda*w(:)'*w(:)/length(w)
% where c(n)=0/1 is the class of the nth trainig input x{n}
%
% Inputs:
% x0 : data matrix from class 0. Each column is a datapoint
% x1 : data from class 1
% opts.lambda : add penalty lambda*sum(w.^2)/length(w) to log likelihood
% opts.w : initial guess for weight vector w
% opts.b : initial guess for bias
% opts.tol : zero gradient convergence criterion
% opts.maxit : maximum number of gradient updates
% opts.plotprogress : show plot of the (penalised) log likelihood
% opts.printprogress : print the (penalised) log likelihood
%
% Outputs:
% w : weight vector
% b : bias
% p10 : probabilities of the class 0 training data to be classfied as class 1
% p11 : probabilities of the class 1 training data to be classfied as class 1
% ul : (non-penalised) log likelihood
% l : penalised log likelihood value

if nargin==3; opts=varargin{1}; else opts=[]; end
opts=setfields(opts,'maxit',10*size(x0,1),'plotprogress',1,'eta',0.1,'printprogress',1,...
    'w',zeros(size(x0,1),1),'b',0,'lambda',0,'tol',10e-5);% default options
eta = opts.eta; % learning rate
w = opts.w; b = opts.b; % initial guess about the parameters
lambda=opts.lambda;
gb = 1; gw = zeros(size(w)); % set gradients initally to ensure at least one update
it = 0; % iteration counter
W=length(w);
while sum(abs(gw)) + abs(gb) > opts.tol  % continue whilst gradient is large
    it = it + 1;  % increment the number of updates carried out
    gb = 0; gw = 0*gw; % reset gradients to zero
    l=0; % log likelihood
    for n = 1:size(x1,2) % cycle through the class 1 data
        %sg = 1/(1+exp(-(b+w'*x1(:,n))))
        sg = exp(mylogsig(b+w'*x1(:,n)));
        c = 1 - sg;
        gb = gb + c;
        gw = gw + c*x1(:,n);
        l = l + logeps(sg);
    end
    for n = 1:size(x0,2) % cycle through the class 0 data
        %sg = 1/(1+exp(-(b+w'*x0(:,n))));
        sg = exp(mylogsig(b+w'*x0(:,n)));
        c = 0 - sg;
        gb = gb + c;
        gw = gw + c*x0(:,n);
        l = l + logeps(1-sg);
    end
    ul=l;
    l=l-lambda*sum(w.^2)/W;
    gw = gw -2*lambda*w/W;
    
    w = w + eta*gw; % update the weight vector
    b = b + eta*gb; % update the bias scalar
    
    if it > opts.maxit; break; end
    L(it)=l;
    if opts.plotprogress
        plot(L); title('log likelihood'); drawnow;
    end
    if opts.printprogress
        fprintf('log likelihood = %g\n',L(end))
    end
    % calculate the probabilities p(c=1|x) for the training data :
    p11=1./(1+exp(-(repmat(b,1,size(x1,2))+w'*x1)));
    p10=1./(1+exp(-(repmat(b,1,size(x0,2))+w'*x0)));
end