
%generate data from a multinomial probit model
%with three choices (and thus work with 
%two utility differences. 

clear;
clc;
randn('seed',sum(100*clock));

%declare conditions for experimental design
nobs = 500;
beta0 = -.5; beta1 = 1;
beta_true = [beta0 beta1]';
nchoice  = 2;
z = zeros(nchoice,2,nobs);
U_true = zeros(nchoice,nobs);
y = zeros(nobs,1);
rho = .4;
sigma2 = .5;
covmat = [sigma2 rho*sqrt(sigma2); rho*sqrt(sigma2) 1];
H = chol(covmat);


%generate the data
for i = 1:nobs;
    z(:,:,i) = [1 randn(1,1); 1 randn(1,1)];
    U_true(:,i) =z(:,:,i)*beta_true + H'*randn(2,1);
    U_max = max(U_true(:,i));
    location = findmin(-U_true(:,i));
    if U_max <0
        y(i,1) = 0;
    else
        y(i,1) = location;
    end;
end;

%input prior hyperparameters
mu_beta = zeros(2,1);
V_beta = (20^2)*eye(2);
invV_beta = inv(V_beta);
rho = 3;
R = eye(2);

%set initial conditions
iter = 1000;
burn = 200;
Sigma = eye(2);
U_use = zeros(nchoice,nobs);
betas_final = zeros(2,iter-burn);
var_final = zeros(iter-burn,1);
corr_final = var_final;

    %define some variables used when sampling the 
    %latent utility data
    %dimensionalize a few matrices
    A = zeros(2,2,3);
    c = zeros(2,3);
    d = zeros(2,3);
    lc = zeros(2,3);
    ld = zeros(2,3);
    
    A(:,:,1) = eye(2);
    A(:,:,2) = [1 0; 1 -1];
    A(:,:,3) = [0 1; -1 1];
    c(:,1) = [-999 -999]';
    c(:,2) = [0 0]';
    c(:,3) = [0 0]';
    lc(:,1) = [1 1]';
    lc(:,2) = [0 0]';
    lc(:,3) = [0 0]';
    d(:,1) = [0 0]';
    d(:,2) = [999 999]';
    d(:,3) = [999 999]';
    ld(:,1) = [0 0]';
    ld(:,2) = [1 1]';
    ld(:,3) = [1 1]';
    
    
%Start the Gibbs sampler (Beta, Sigma, Latent Data)        

clear i;
for i = 1:iter;
    i
    %-----------
    %sample beta
    %-----------
    part1 = zeros(2,2);
    part2 = zeros(2,1);
    
    for j = 1:nobs;
        temp1 = z(:,:,j)'*inv(Sigma)*z(:,:,j);
        temp2 = z(:,:,j)'*inv(Sigma)*U_use(:,j);
        part1 = part1 + temp1;
        part2 = part2 + temp2;
    end;
    D_beta = inv(part1 + invV_beta);
    d_beta = part2 + invV_beta*mu_beta;
    HD_beta = chol(D_beta);
    betas = D_beta*d_beta + HD_beta'*randn(2,1)
    
    %----------------
    %sample Sigma^{-1}
    %-----------------  
    errors = zeros(nchoice,nobs);
    for j = 1:nobs;
        errors(:,j) = U_use(:,j) - z(:,:,j)*betas;
    end;
    error_part = errors*errors';
    Sigma_inv = nobile_wishart(nobs+rho,(rho*R + error_part));
    Sigma = inv(Sigma_inv);
    
    %------------------
    %sample latent data
    %-------------------
    for j = 1:nobs;
        index_use = y(j) +1;
        mean_use = z(:,:,j)*betas;
        U_use(:,j) = tnorm_rnd(2,mean_use,Sigma,c(:,index_use),d(:,index_use),lc(:,index_use), ld(:,index_use),A(:,:,index_use),[1;2]);
    end;
    
    
    if i > burn;
        betas_final(:,i-burn) = betas;
        var_final(i-burn,1) = Sigma(1,1);
        corr_final(i-burn,1) = Sigma(1,2)/sqrt(Sigma(1,1));
    end;
end;


mean(betas_final')
std(betas_final')
mean(var_final)
std(var_final)
mean(corr_final)
std(corr_final)

    