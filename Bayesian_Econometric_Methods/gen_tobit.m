%This is a new go at the gaussian selection model,
%leading to the mixture of normals model. 

%D* = theta_0 + theta_1Z + u_D
%Y^1 = beta_1 + u_1
%Y^0 = beta_0 + u^0
% Covmat = [1           sigma_1D    sigma_0D
%           sigma_1D    sigma_1^2   sigma_0D
%           sigma_0D    sigma_10    sigma_0^2]

clear;
clc;
randn('seed',sum(100*clock));

%---------------------------------------------
%Set the initial parameters values used in the 
%generated data
%---------------------------------------------
max_iter = 2;
ate_point = zeros(max_iter,1);
ate_std = zeros(max_iter,1);
%for jjj = 1:max_iter;

theta_0 = 0;    theta_1 = .5;
beta_1 = 2;     %two intercepts
beta_0 = 1;

sigma_1 = 1;   %these are std. deviation parms.
sigma_0 = 1;
corr1D = .9;
corr0D = .7;
corr10 = .6;        %non-id covariance parameter.
cov1D = corr1D*sigma_1;
cov0D = corr0D*sigma_0;
cov10 = corr10*sigma_1*sigma_0 
Sigma = [1      cov1D       cov0D;
        cov1D   sigma_1^2    cov10;
        cov0D   cov10      sigma_0^2];
H = chol(Sigma);

%-----------------
%Generate the data
%-----------------

nobs = 5000;
z = randn(nobs,1);

%generate the data
for i = 1:nobs;
    resids = H'*randn(3,1);
    index = theta_0 + theta_1*z(i,1) + resids(1,1);
    D(i,1) = .5*sign(index) + .5;
    y1(i,1) = beta_1  + resids(2,1);
    y0(i,1) = beta_0  + resids(3,1);
    tempp(:,i) = resids;
end;

y = D.*y1 + (1-D).*y0;

xmat = [ones(nobs,1) ];
zmat = [ones(nobs,1) z];

kbar = size(xmat,2) + size(xmat,2) + size(zmat,2);
kz = size(zmat,2);
kx = size(xmat,2);
%-------------------------------------------------
%CHOOSE THE PRIOR SPECIFICATION
%-------------------------------------------------
mu_beta1 = [0]';     V_beta1 = 10^2*eye(size(xmat,2));
mu_beta0 = [0]';     V_beta0 = 10^2*eye(size(xmat,2));
mu_theta = [0 0]';  V_theta = 10^2*eye(size(zmat,2));
rho = 4;
%R=[sigma_0^2 cov10 cov0D; cov10 sigma_1^2 cov1D; cov0D cov1D 1];
%R = [sigma_0^2 0 0; 0 sigma_1^2 0; 0 0 1]
R= eye(3);

covarian = blockdiag(V_theta,V_beta1,V_beta0);
inv_parms = inv(covarian);
big_mean = [mu_theta; mu_beta1; mu_beta0];

%--------------------------------------------------
%CONSTRUCT SOME MATRICIES TO BE USED LATER
%--------------------------------------------------
ZZ = zmat'*zmat;
ZX = zmat'*xmat;
XX = xmat'*xmat;
XZ = xmat'*zmat;             
%-------------------------------------------------
%SET VECTOR, MATRIX LENGTHS AND INITIAL CONDITIONS.
%-------------------------------------------------
iter = 5500;
burn = 500;

beta1_final = zeros(size(xmat,2),iter-burn);
beta0_final = beta1_final;
theta_final = zeros(size(zmat,2),iter-burn);
sig1_final = zeros(iter-burn,1);
sig0_final = sig1_final;
corr1_final = sig1_final;
corr0_final = sig1_final;
corr10_final = sig1_final;

Dstar = truncnorm(0,1,D);
Sigma = Sigma;
Siginv = inv(Sigma);

sig1= sqrt(Sigma(2,2));
sig0 = sqrt(Sigma(3,3));
sig1D = Sigma(1,2);
sig0D = Sigma(1,3);
sig10 = Sigma(2,3);
beta1 = [.5]';
beta0 = [.5]';
theta = [0 0]';

%-----------------------
%BEGIN THE GIBBS SAMPLER
%-----------------------

for j = 2:iter;
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%
    %Draw the missing outcome%
    %%%%%%%%%%%%%%%%%%%%%%%%%%
    
    denom0 = (sig0^2) - (sig0D^2);
    denom1 = (sig1^2) - (sig1D^2);
    middle = sig10*sig0D*sig1D;
    
    mu_1 = xmat*beta1 + (Dstar - zmat*theta)*( ( (sig0^2)*sig1D - sig10*sig0D)/denom0 ) + ...
            (y - xmat*beta0)*( (sig10 - sig0D*sig1D)/denom0 );
    mu_0 = xmat*beta0 + (Dstar - zmat*theta)*( ( (sig1^2)*sig0D - sig10*sig1D)/denom1 ) + ...
            (y - xmat*beta1)*( (sig10 - sig0D*sig1D)/denom1 );
    
    omega_1 = (sig1^2) - ( ( (sig1D^2)*(sig0^2) - 2*middle + (sig10^2))/denom0 );
    omega_0 = (sig0^2) - ( ( (sig0D^2)*(sig1^2) - 2*middle + (sig10^2))/denom1 );
    
    mu_miss = (1-D).*mu_1 + (D).*mu_0;
    var_miss = (1-D)*omega_1 + (D)*omega_0;
    
    y_miss = mu_miss + sqrt(var_miss).*randn(nobs,1);
    
    %%%%%%%%%%%%%%%%%%%%%%
    %Draw the latent data%
    %%%%%%%%%%%%%%%%%%%%%%
    
    denomd = (sig1^2)*(sig0^2) - (sig10^2);
    
    mu_d = zmat*theta + (D.*y + (1-D).*(y_miss) - xmat*beta1)*( ((sig0^2)*sig1D - sig10*sig0D)/denomd ) + ...
            (D.*y_miss + (1-D).*y - xmat*beta0)*( ( (sig1^2)*sig0D - sig10*sig1D)/denomd );
    omega_d = 1 - ( ( (sig1D^2)*(sig0^2) - 2*sig10*sig0D*sig1D + (sig1^2)*(sig0D^2))/ denomd );       
    
    Dstar = truncnorm(mu_d,omega_d,D);
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Draw the Coefficient Vectors%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    s11 = Siginv(1,1);      s12 = Siginv(1,2);          s13 = Siginv(1,3);
    s21 = Siginv(2,1);      s22 = Siginv(2,2);          s23 = Siginv(2,3);
    s31 = Siginv(3,1);      s32 = Siginv(3,2);          s33 = Siginv(3,3);
    
    yuse = [Dstar' (D.*y + (1-D).*y_miss)' ((1-D).*y + (D).*y_miss)']';
    
    zmata = [(zmat'*s11) (zmat'*s12) (zmat'*s13)];
    x1mata = [(xmat'*s21) (xmat'*s22) (xmat'*s23)];
    x0mata = [(xmat'*s31) (xmat'*s32) (xmat'*s33)];

    xmatpart = [ (ZZ*s11) 	(ZX*s12)  	(ZX*s13); ... 
          		 (XZ*s21)	(XX*s22)	(XX*s23); ...
                 (XZ*s31)  (XX*s32) 	(XX*s33)];
       
    ymatpart = [zmata*yuse; x1mata*yuse; x0mata*yuse];
    covmatpart = inv(xmatpart + inv_parms);
    meanpart = covmatpart*(ymatpart + inv_parms*big_mean);

    betas = meanpart + (chol(covmatpart))'*randn((kbar),1);

    theta = betas(1:kz);
    beta1 = betas(kz+1:kz+kx);
    beta0 = betas(kz+kx+1:kbar);

   % [beta1 beta0 theta']
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Draw the Covariance Matrix%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    y_one = D.*y + (1-D).*y_miss;
    y_zero = D.*y_miss + (1-D).*y;
    
    e_D = Dstar - zmat*theta;
    e_1 = y_one - xmat*beta1;
    e_0 = y_zero - xmat*beta0;
    
    term3 = [   (e_0'*e_0)      (e_0'*e_1)      (e_0'*e_D);
                (e_1'*e_0)      (e_1'*e_1)      (e_1'*e_D);
                (e_D'*e_0)      (e_D'*e_1)      (e_D'*e_D)];
    
            %put in this order for the purposes of the Nobile 
            %algorithm. For that algorithm, we need to have 
            %the normalization be at the (3,3) element. Then, 
            %we need to be sure to pick of the covariance 
            %terms correctly, as we do below.
            %
            %Note also that the Nobile algorithm we are 
            %using directly outputs a Draw for Sigma instead 
            %of the usual Sigma^-1 draw. This draw for Sigma
            %conditions on the (3,3) element being equal to 1.
            
    Sig_temp = nobile_wishart3(nobs + rho, (term3 + rho*R));
    Sig = [Sig_temp(3,3) Sig_temp(3,2) Sig_temp(3,1);
           Sig_temp(2,3) Sig_temp(2,2) Sig_temp(2,1);
          Sig_temp(1,3) Sig_temp(1,2) Sig_temp(1,1)];
     %  Sig = [Sig_temp(3,3) corr1D*sqrt(Sig_temp(2,2)) corr0D*sqrt(Sig_temp(1,1));          %replace corr's with D with true values
     %      corr1D*sqrt(Sig_temp(2,2)) Sig_temp(2,2) Sig_temp(2,1);                  
     %      corr0D*sqrt(Sig_temp(1,1)) Sig_temp(1,2) Sig_temp(1,1)];
    %Sig_temp = Sigma;
    %Sig = Sig_temp;
  
    Siginv = inv(Sig);
       
    sig1 = sqrt(Sig(2,2));
    sig0 = sqrt(Sig(3,3));
    sig10 = Sig(2,3);
    sig1D = Sig(1,2);
    sig0D = Sig(1,3);
    
   % [sig1 sig0 sig1D/sig1 sig0D/sig0]
    
    if j >burn
        beta1_final(:,j-burn) = beta1;
        beta0_final(:,j-burn) = beta0;
        theta_final(:,j-burn) = theta;
        sig1_final(j-burn,1) = sig1;
        sig0_final(j-burn,1) = sig0;
        corr1_final(j-burn,1) = sig1D/sig1;
        corr0_final(j-burn,1) = sig0D/sig0;
        corr10_final(j-burn,1) = sig10/(sig1*sig0);    
    end;
end;

disp('Posterior Means, and True Values of Regression parms -STD ERRORS at END');
[beta_1  beta_0 theta_0 theta_1; 
    mean(beta1_final') mean(beta0_final') mean(theta_final(1,:)) mean(theta_final(2,:)); 
    std(beta1_final') std(beta0_final') std(theta_final(1,:)) std(theta_final(2,:))]   
   
disp('Posterior Means, and True Values of Covariance PArms - STD ERRORS at END');
[sigma_1 sigma_0 corr1D corr0D corr10; 
  mean(sig1_final) mean(sig0_final) mean(corr1_final) mean(corr0_final) mean(corr10_final) ; 
  std(sig1_final) std(sig0_final) std(corr1_final) std(corr0_final) std(corr10_final)  ]

%Compare the point estimates to OLS ones;
points1 = find(D==1);
xuse = xmat(points1,:);  yuse = y(points1);
[bhat1 stderr1 tstat1 sig1 rsq1] = ols(xuse,yuse);
clear xuse yuse;

points0 = find(D==0);
xuse = xmat(points0,:); yuse = y(points0,:);
[bhat0 stderr0 tstat0 sig0 rsq0] = ols(xuse,yuse);
disp('OLS Estimation of Outcome Intercepts - Treated, Untreated');
[bhat1 bhat0]

%Plot the priors and posteriors for the Non-ID correlation parameter
iter2 = 10000;
nonid_corr = zeros(iter2,1);
id_corr1D = zeros(iter2,1);
id_corr0D = zeros(iter2,1);
for i = 1:iter2;
    tempp = nobile_wishart3(rho,rho*R);
    temppp = [tempp(3,3) tempp(3,2) tempp(3,1);
                    tempp(2,3) tempp(2,2) tempp(2,1);
                    tempp(1,3) tempp(1,2) tempp(1,1)];
    sig1t = sqrt(temppp(2,2));
    sig0t = sqrt(temppp(3,3));
    sig10t = temppp(2,3);
    sig1Dt = temppp(1,2);
    sig0Dt = temppp(1,3);
    
    nonid_corr(i,1) = sig10t/(sig1t*sig0t);
    id_corr1D(i,1) = sig1Dt/(sig1t);
    id_corr0D(i,1) = sig0Dt/(sig0t);
end;

[dom,ran] = epanech2(nonid_corr);
[dom2,ran2] = epanech2(corr10_final);

plot(dom,ran,':');
hold on;
plot(dom2,ran2);
xlabel('\rho_{10}');
ylabel('Density');
