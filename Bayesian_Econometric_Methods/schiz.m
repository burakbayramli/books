%This m-file runs a simple two-component model on 
%the mixture response data
clear; clc;
randn('seed',sum(100*clock));
rand('seed',sum(100*clock));

load responses.txt;
log_data = log(responses);
T = mean(log_data')';
nobs = length(T);
    %this creates a 17 times 1 vector of mean response times,
    %the first 11 of which are not schizophrenic!
    
    %set up the priors
    mu10 = 5.5;
    V_mu1 = .2^2;
    alpha0 = .4;
    V_alpha = .2^2;
    a1 = 2; a2 = 2;
    b1 = 1/(.1^2); b2 = 1/(.1^2);
   beta0 = [0 0]';
    V_beta = 4*eye(2);
   % beta0= 0;
    %V_beta =4;
    %set up the initial conditions
    iter = 50000;
    burn = 1000;
    mu1 = zeros(iter,1);
    alpha = zeros(iter,1);
    sig1 = mu1;
    sig2 = mu1;
    betas = zeros(iter,2);
   mu1_final = zeros(iter-burn,1);
   alpha_final = mu1_final;
   sig1_final = mu1_final;
   sig2_final = mu1_final;
   betas_final = zeros(iter-burn,2);
    %betas = zeros(iter,1);
    xmat = [ones(nobs,1) [zeros(1,11) ones(1,6)]' ];
  % xmat = ones(nobs,1);
   z = [zeros(1,11) ones(1,6)]';       %z=1 for the schizophrenics (second component)
    alpha(1) = 1;
    sig1(1) = 1; sig2(1) = 1;
    
    zstar = truncnorm(zeros(17,1),.1,z);
    z_counter = zeros(nobs,1);
    %run the gibbs sampler
    for i = 2:iter;
    %------------------------
    %do mean parameters of first component
    %-----------------------
    
    %Mu1   
    t_tilde = T - z*alpha(i-1);
    vec_temp = (1-z)*sig1(i-1) + z*sig2(i-1);
    Sigma = diag(vec_temp);
    Sig_inv = inv(Sigma);
    
    D_mu1 = inv(sum( (vec_temp).^(-1)) + inv(V_mu1));
    d_mu1 = sum( Sig_inv*t_tilde) + inv(V_mu1)*mu10;
    mu1(i) = D_mu1*d_mu1 + sqrt(D_mu1)*randn(1,1);
    
    %alpha
    points = find(zstar>0);
    num_2 = length(points);
    Tuse = T(points);
    t_tilde = Tuse - mu1(i);
    D_alpha = inv(num_2/sig2(i-1) + inv(V_alpha));
    d_alpha = sum(t_tilde)/sig2(i-1) + inv(V_alpha)*alpha0;
    alpha(i) = truncnorm(D_alpha*d_alpha,D_alpha,1);
    
    %sigma1^2
    points = find(zstar<=0);
    num_1 = length(points);
    T_use = T(points);
    sig1(i) = invgamrnd( (num_1/2) + a1, inv( inv(b1) + .5*sum( (T_use - mu1(i)).^2 ) ),1,1);
    
    %sigma2^2
    points = find(zstar>0);
    num_2 = length(points);
    T_use = T(points);
    sig2(i) = invgamrnd( (num_2/2) + a2, inv( inv(b2) + .5*sum( (T_use - mu1(i) - alpha(i)).^2 ) ),1,1);
    
    %beta0, beta1
    D_beta = inv(xmat'*xmat + inv(V_beta));
    d_beta = xmat'*zstar + inv(V_beta)*beta0;
    H = chol(D_beta);
    betas(i,:) = (D_beta*d_beta + H'*randn(2,1))';
    %betas(i,:)
    %betas(i,:) = (D_beta*d_beta + H'*randn(1,1))';

    %zstar
    PHI = 1-normcdf(xmat*betas(i,:)');
    norm1 = normpdf(T,mu1(i),sqrt(sig1(i)));
    norm2 = normpdf(T,mu1(i) + alpha(i),sqrt(sig2(i)));
    prob1 = (norm1.*PHI)./( norm1.*PHI + norm2.*(1-PHI));
    uniforms = rand(nobs,1);
    U = .5*sign(prob1 - uniforms) + .5; %this indicates the probability of first (non-delayed) component!
    z = 1-U; %convert U to negative values when U=1
    zstar = truncnorm(xmat*betas(i,:)',ones(nobs,1),z);
 
    
    if i > burn
        mu1_final(i-burn,1) = mu1(i);
        alpha_final(i-burn,1) = alpha(i);
        betas_final(i-burn,:) = betas(i,:);
        sig1_final(i-burn,1) = sqrt(sig1(i));
        sig2_final(i-burn,1) = sqrt(sig2(i));
        z_counter = z_counter + z;    
    end;
    
end;
clc;
mean(mu1_final)
mean(alpha_final)
mean([sig1_final sig2_final])
disp('means of prob. of components 1,2 S=1');
mean([ (1-normcdf(betas_final(:,1) + betas_final(:,2), zeros(iter-burn,1), ones(iter-burn,1))) ...
    (normcdf(betas_final(:,1) + betas_final (:,2), zeros(iter-burn,1), ones(iter-burn,1)))])
disp('means of prob. of components 1,2 S=0');
mean([ (1-normcdf(betas_final(:,1),zeros(iter-burn,1),ones(iter-burn,1))) ...
    (normcdf(betas_final(:,1),zeros(iter-burn,1),ones(iter-burn,1)))])
disp('Probabilities that each belongs to the Delayed component');
z_counter/(iter-burn)