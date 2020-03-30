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
muvec = logspace(-30,10,60);

% For plotting purposes
xx = pickpoints(-1,1,300);
yy = yf(xx);

% Pick a shape parameter and form the design matrix
% The relevant computations are done with the SVD
% This is only for stability and can be changed for larger sizes
ep = .1;
gqr_alpha = 1;
K_fit = rbf(ep,DistanceMatrix(x,z));
K_predict = rbf(ep,DistanceMatrix(xx,z));
[UK,SK,VK] = svd(K_fit,0);SKv = diag(SK);

% Also, form necessary GQR stuff
% Note that we are feeding this the centers, with which to form the stable
% basis, and also the eigenfunction basis for comparison
GQR = gqr_solveprep(0,z,ep,gqr_alpha);
Psi = gqr_phi(GQR,x)*[eye(M);GQR.Rbar];
[UPsi,SPsi,VPsi] = svd(Psi,0);SPsiv = diag(SPsi);
% This is the eigenfunction basis
GQR_reg = gqr_solveprep(1,z,ep,gqr_alpha,M);
Phi_reg = gqr_phi(GQR_reg,x);
[UPhi,SPhi,VPhi] = svd(Phi_reg,0);SPhiv = diag(SPhi);

% Conduct the loop over the regularization values
dirvec = [];gqrvec = [];eigvec = [];
loovec = [];loevec = [];
gcdvec = [];gcevec = [];
eig_err_best = Inf;dir_err_best = Inf;gce_err_best = Inf;gcd_err_best = [];
k = 1;
for mu=muvec
    % Solve for the newtork weights, here with the standard basis
    w = VK*((UK'*y)./(SKv+mu./SKv));

    % Evaluate predictions on the test/plotting points
    % Check the error
    yp = K_predict*w;
    dirvec(k) = errcompute(yp,yy);

    % Find the projection matrix, used to evaluated the residual
    P = eye(N) - UK*diag(1./(1+mu./SKv.^2))*UK';
    Py = P*y;

    % Evaluate the parameterization schemes
    % The projection matrix is needed for this as well
    lodvec(k) = Py'*diag(1./diag(P).^2)*Py/N;
    gcdvec(k) = N*Py'*Py/trace(P)^2;
    
    % Compute instead the coefficients for the HS-SVD method
    % We will first compute with the stable basis
    GQR.coef = VPsi*((UPsi'*y)./(SPsiv+mu./SPsiv));
    yp = gqr_eval(GQR,xx);
    gqrvec(k) = errcompute(yp,yy);
    
    % Compute as well with the eigenfunctions
    % First the CV content for the eigenfunction basis
    % Then the predictions at the test points
    GQR_reg.coef = VPhi*((UPhi'*y)./(SPhiv+mu./SPhiv));
    P = eye(N) - UPhi*diag(1./(1+mu./SPhiv.^2))*UPhi';
    Py = P*y;
    gcevec(k) = N*Py'*Py/trace(P)^2;
    loevec(k) = Py'*diag(1./diag(P).^2)*Py/N;
    yp = gqr_eval(GQR_reg,xx);
    eigvec(k) = errcompute(yp,yy);

    if eigvec(k)<eig_err_best
        eig_err_best = eigvec(k);eig_mu_best = mu;eig_y_best = yp;
    end
    if dirvec(k)<dir_err_best
        dir_err_best = dirvec(k);dir_mu_best = mu;dir_y_best = yp;
    end
    if gcevec(k)<gce_err_best
        gce_err_best = gcevec(k);gce_mu_best = mu;gce_y_best = yp;
    end
    if gcdvec(k)<gcd_err_best
        gcd_err_best = gcdvec(k);gcd_mu_best = mu;gcd_y_best = yp;
    end
    k = k + 1;
end

[tmp,id] = min(dirvec);
[tmp,ig] = min(gcdvec);
[tmp,ie] = min(eigvec);
[tmp,ic] = min(gcevec);
figure
handles(1) = loglog(muvec,dirvec,'linewidth',3);
hold on
handles(2) = loglog(muvec,gcdvec,'--','linewidth',3);
handles(3) = loglog(muvec,eigvec,'r','linewidth',3);
handles(4) = loglog(muvec,gcevec,'--r','linewidth',3);
loglog(muvec(id),dirvec(id),'x','linewidth',3,'markersize',16)
loglog(muvec(ig),gcdvec(ig),'x','linewidth',3,'markersize',16)
loglog(muvec(ie),eigvec(ie),'r+','linewidth',3,'markersize',16)
loglog(muvec(ic),gcevec(ic),'r+','linewidth',3,'markersize',16)
handles(5) = loglog(muvec(1:2:end),gqrvec(1:2:end),'ok','linewidth',1);
title(sprintf('ep=%g,N=%d,M=%d',ep,N,M))
xlabel('\mu')
legend(handles,'Standard Basis','Standard GCV',...
       'Eigenfunction Basis','Eigenfunction GCV',...
       'Stable Basis','location','northwest')
hold off

% This can compute the mu=0 error, which I guess should be mu now that
% I think about it
GQR_reg.coef = Phi_reg\y;
yp = gqr_eval(GQR_reg,xx);
mu0_err = errcompute(yp,yy);

% Plot the results
figure
plot(x,y,'or')
hold on
plot(xx,yy,'linewidth',2)
plot(xx,eig_y_best,'--k','linewidth',2)
plot(xx,dir_y_best,'-.m','linewidth',2)
hold off
ylim([-1,2])
title(sprintf('ep=%g,mu=%g,N=%d,M=%d',ep,eig_mu_best,N,M))
legend('Data','True',...
    sprintf('Eig Opt err=%2.2g',eig_err_best),...
    sprintf('Stand Opt err=%2.2g',dir_err_best),...
    'location','south')