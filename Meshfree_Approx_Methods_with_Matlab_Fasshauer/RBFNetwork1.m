% RBFNetwork1
% This first example considers Tikhonov Regularization for fixed centers
% and shape parameters

% Initial example for support-vector machines
if exist('rng','builtin')
    rng(0);
else
    rand('state',0);
    randn('state',0);
end

% Testing data
N = 50;
M = 15;
yf = @(x) (1-4*x+32*x.^2).*exp(-16*x.^2);
rbf = @(e,r) exp(-(e*r).^2);

% Pick points to evaluate the function at
% Add some error to the data
x = pickpoints(-1,1,N-2,'rand');
x = [x;-1;1];
noise = .2;
y = yf(x) + noise*randn(N,1);

% Choose points at which to center the basis functions, as needed
z = pickpoints(-1,1,M,'halton');

% Pick a range of Tikhonov Regularization parameters
muvec = [logspace(-10,-5,5),logspace(-4.9,5,50)];

% For plotting purposes
xx = pickpoints(-1,1,300);
yy = yf(xx);

% Pick a shape parameter and form the design matrix
% The relevant computations are done with the SVD
% This is only for stability and can be changed for larger sizes
ep = 8;
K_fit = rbf(ep,DistanceMatrix(x,z));
K_predict = rbf(ep,DistanceMatrix(xx,z));
[U,S,V] = svd(K_fit,0);Sv = diag(S);

% Conduct the loop over the regularization values
errvec = [];
loovec = [];
gcvvec = [];
err_min = Inf;gcv_min = Inf;loo_min = Inf;
k = 1;
for mu=muvec
    % Solve for the newtork weights, here with the standard basis
    w = V*((U'*y)./(Sv+mu./Sv));

    % Evaluate predictions on the test/plotting points
    % Check the error
    yp = K_predict*w;
    errvec(k) = errcompute(yp,yy);

    % Find the projection matrix, used to evaluated the residual
    P = eye(N) - U*diag(1./(1+mu./Sv.^2))*U';
    Py = P*y;

    % Evaluate the parameterization schemes
    % The projection matrix is needed for this as well
    loovec(k) = Py'*diag(1./diag(P).^2)*Py/N;
    gcvvec(k) = N*(Py'*Py)/trace(P)^2;
    if errvec(k)<err_min
        err_min = errvec(k);err_mu_best = mu;err_y_best = yp;
    end
    if gcvvec(k)<gcv_min
        gcv_min = gcvvec(k);gcv_mu_best = mu;gcv_y_best = yp;
    end
    if loovec(k)<loo_min
        loo_min = loovec(k);loo_mu_best = mu;loo_y_best = yp;
    end
    k = k + 1;
end

[tmp,id] = min(errvec);
[tmp,ig] = min(gcvvec);
[tmp,il] = min(loovec);
handles = [];
figure
handles(1) = loglog(muvec,errvec,'k','linewidth',3);
hold on
handles(2) = loglog(muvec,gcvvec,'--','linewidth',3);
handles(3) = loglog(muvec,loovec,'-.','color',[.7 .5 0],'linewidth',3);
loglog(muvec(id),errvec(id),'k+','linewidth',3,'markersize',16)
loglog(muvec(ig),gcvvec(ig),'x','linewidth',3,'markersize',16)
loglog(muvec(il),loovec(il),'x','color',[.7 .5 0],'linewidth',3,'markersize',16)
title(sprintf('ep=%g,N=%d,M=%d',ep,N,M))
xlabel('\mu')
legend(handles,'Error','GCV','LOOCV','location','northwest')
hold off

% Store the error (not the min) for the best GCV
gcv_min_err = errcompute(gcv_y_best,yy);

% Plot the results
figure
plot(x,y,'or')
hold on
plot(xx,yy,'r','linewidth',2)
plot(xx,err_y_best,'k','linewidth',2)
plot(xx,gcv_y_best,'--','linewidth',2)
hold off
ylim([-.5,2])
title(sprintf('ep=%g,mu=%g,N=%d,M=%d',ep,gcv_mu_best,N,M))
legend('Data','True',...
    sprintf('Min Error err=%2.2g',err_min),...
    sprintf('Min GCV err=%2.2g',gcv_min_err),...
    'location','northeast')