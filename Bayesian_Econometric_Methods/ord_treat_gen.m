%This m-file provides a gibbs algorithm for fitting 
%an ordered-treatment, ordered outcome model. 
clear;
clc;
randn('seed',sum(100*clock));

load health_clean;

nobs = length(y);

%--------------
%PRIORS
%----------------
a = 2;
H = eye(2);
Vy = 10*eye(size(xbary,2));
Vr = 10*eye(size(xr,2));
V = blockdiag(Vy,Vr);


iter = 500;
burn = 100;

gamma3_final = zeros(iter-burn,1);
tgamma3_final = zeros(iter-burn,1);
by_final = zeros(iter-burn,size(xbary,2));
br_final = zeros(iter-burn,size(xr,2));
syr_final = zeros(iter-burn,1);

zstary = randn(nobs,1);
zstarr = randn(nobs,1);
Sigma_star_inv = eye(2);
Sigma_star = inv(Sigma_star_inv);
sigy = Sigma_star(1,1);
sigr = Sigma_star(2,2);
tildesigyr = Sigma_star(1,2);

for i = 1:iter;
    
    %---------------
    %Sample Regression parameters
    %---------------
        xterm = zeros(size(xbary,2) + size(xr,2),size(xbary,2) + size(xr,2));
        yterm = zeros(size(xbary,2) + size(xr,2),1);
        for j = 1:nobs;
            X_j = blockdiag(xbary(j,:),xr(j,:));
            z_j = [zstary(j,1) zstarr(j,1)]';
            tempp_X = X_j'*Sigma_star_inv*X_j;
            tempp_Y = X_j'*Sigma_star_inv*z_j;
            xterm = xterm + tempp_X;
            yterm = yterm + tempp_Y;
        end;
    D_beta = inv(xterm + inv(V));
    d_beta = yterm;
    H_beta = chol(D_beta);
    beta_star = D_beta*d_beta + H_beta'*randn(size(xbary,2) + size(xr,2),1);
    betay_star = beta_star(1:size(xbary,2));
    betar_star = beta_star(size(xbary,2)+1:length(beta_star));

    %------------------
    %Sample latent data in outcome equation
    %-------------------
    muy_r = xbary*betay_star + tildesigyr*inv(sigr)*(zstarr - xr*betar_star);
    sigy_r = (sigy - (tildesigyr^2)*inv(sigr))*ones(nobs,1);
        for j = 1:nobs;
            zstary(j,1) = truncnorm2(muy_r(j),sigy_r(j),a_y(j),b_y(j));
        end;

    %-----------------
    %Sample latent data in treatment equation
    %-----------------
    mur_y = xr*betar_star + tildesigyr*inv(sigy)*(zstary - xbary*betay_star);
    sigr_y = (sigr - (tildesigyr^2)*inv(sigy))*ones(nobs,1);
        for j = 1:nobs;
            zstarr(j,1) = truncnorm2(mur_y(j),sigr_y(j),a_r(j),b_r(j));
        end;
        
   
    %-----------------
    %Sample inverse coavariance matrix
    %------------------
    residsy = zstary - xbary*betay_star;
    residsr = zstarr - xr*betar_star;
    innermat = [ (residsy'*residsy) (residsy'*residsr); (residsr'*residsy) (residsr'*residsr)];
    Sigma_star_inv = wishrnd( inv(inv(H) + innermat),nobs + a);
    Sigma_star = inv(Sigma_star_inv);
    
    
    %-------------
    %Transform back into parameters of interest
    %-------------
    sigy = Sigma_star(1,1);
    sigr = Sigma_star(2,2);
    tildesigyr = Sigma_star(1,2);
    
  
    
    gamma3a = sqrt(1/sigy);
    tgamma3a = sqrt(1/sigr);
    by = gamma3a*betay_star;
    br = tgamma3a*betar_star;
    syr = gamma3a*tgamma3a*tildesigyr;
 
        if i > burn;
            gamma3_final(i-burn,1) = gamma3a;
            tgamma3_final(i-burn,1) = tgamma3a;
            by_final(i-burn,:) = by';
            br_final(i-burn,:) = br';
            syr_final(i-burn,1) = syr;
        end;
    end;
    clc;
    disp('xbary = [education havekid_before famincome healthprob r(1) r(2) r(3)'];
    disp('xr= [ones(nobs,1) education famincome parent_drink]');
    disp('Post Means and Std. Dev in Drinking Freq. Equation');
    [mean(br_final)' std(br_final)']
    disp('Post Means and Std. Dev in Doctor Visit Equation');
    [mean(by_final)' std(by_final)']
    disp('Post. Mean and Std. Dev of Cutpoint in Drinking Equation');
    [mean(tgamma3_final) std(tgamma3_final)]
    disp('Post. Mean and Std. Dev of Cutpoint in Doctor Visit Equation');
    [mean(gamma3_final) std(gamma3_final)]
    disp('Post. Mean and Std. Dev. of correlation parameter - sigma_yr');
    [mean(syr_final) std(syr_final)]