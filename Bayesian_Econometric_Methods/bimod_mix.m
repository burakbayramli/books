
clear;
clc;
randn('seed',sum(100*clock));
rand('seed',sum(100*clock));

%------------------------------------------------------------------
%generate normal mixture data
%------------------------------------------------------------------
m1 = 0;
m2 = 2;
s1 = 1;
s2 = .5;
Puse = .4;     %first component
nobs = 3000;
for j = 1:nobs;
    unif = rand(1,1);
    if unif <= Puse
        y(j,1) = m1 + sqrt(s1)*randn(1,1);
    else
        y(j,1) = m2 + sqrt(s2)*randn(1,1);
    end;
end;
%ygrid = linspace(-2,5,500)';
%true_density = Puse*normpdf(ygrid,m1,sqrt(s1)) + (1-Puse)*normpdf(ygrid,m2,sqrt(s2));
%plot(ygrid,true_density);
%pause;
xmat = [ones(nobs,1)];
%-------------------------------------------------
%CHOOSE THE PRIOR SPECIFICATIONS
%-------------------------------------------------

n_comp = 2;
alphas = 2*ones(n_comp,1);

mu_beta1 = [0]';     mu_beta2 = 3;
V_beta1 = 4*eye(size(xmat,2)); V_beta2 = 4*eye(size(xmat,2));

beta0 = [mu_beta1 mu_beta2];
inv_parms = [V_beta1 V_beta2];

a = [3 3]';
b = [(1/2) (1/2)]';

%-------------------------------------------------
%SET VECTOR, MATRIX LENGTHS AND INITIAL CONDITIONS.
%-------------------------------------------------
iter = 2000;
burn = 500;

beta1_final = zeros(iter-burn,n_comp);
sig_final = zeros(iter-burn,n_comp);

P_final = zeros(iter-burn,1);


%start out with tau = the true division of the components.
P = [.5 .5];
betas = [6 6];
sig_temp = [2 2];
tempp = .5*sign(rand(nobs,1)-.5) + .5;
tau = [tempp (1-tempp)]; 
clear tempp;

%-----------------------
%BEGIN THE GIBBS SAMPLER
%-----------------------

for j = 2:iter;
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Draw the Coefficient Vectors%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    for l = 1:n_comp;
        points = find(tau(:,l)==1);
        x_use = xmat(points,:);
        y_use = y(points);
       
    D_beta = inv(x_use'*x_use/sig_temp(:,l) + inv_parms(:,l));
    d_beta = x_use'*y_use/sig_temp(:,l) + inv_parms(:,l)*beta0(:,l);
    H = chol(D_beta);
    betas(:,l) = D_beta*d_beta + H'*randn(size(xmat,2),1);
    clear points x_use y_use D_beta d_beta H;
end;

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Draw the VARIANCE PARAMETERS%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    for l = 1:n_comp;
        points = find(tau(:,l)==1);
        x_use = xmat(points,:);
        y_use = y(points);
  
        resids = .5*(y_use - betas(:,l))'*(y_use - betas(:,l));
        sig_temp(:,l) = invgamrnd( (length(y_use)/2 + a(l,1)), inv(resids + inv(b(l,1))),1,1);
  end;
    %%%%%%%%%%%%%%%%%%%%%%%%%%
    %SAMPLE THE COMPONENT LABEL VECTOR%
    %%%%%%%%%%%%%%%%%%%%%%%%%%
   tempp1 = normpdf(y,betas(:,1),sqrt(sig_temp(:,1)));
   tempp2 = normpdf(y,betas(:,2),sqrt(sig_temp(:,2)));
    
    probs_1 = P(1)*tempp1;
    probs_2 = P(2)*tempp2;
    normalize = probs_1 + probs_2;
    
    tempp = rand(nobs,1);
    tau = [((.5*sign(probs_1./normalize - tempp)+.5)) (1 - (.5*sign(probs_1./normalize - tempp)+.5))];
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %SAMPLE THE COMPONENT PROBABILTIES%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    nn = sum(tau);
    P = dirchlet_rnd([(nn(1) + alphas(1)) (nn(2) + alphas(2))]);
    
    
    if j >burn
        beta1_final(j-burn,:) = [betas(:,1) betas(:,2)];    
        P_final(j-burn) = P(1);
        sig_final(j-burn,:) = [sig_temp(:,1) sig_temp(:,2)];
   end;

end;

ygrid = linspace(-2,5,500)';

for j = 1:length(ygrid);
density(j,1) = mean ((P_final(:,1)).*normpdf(ygrid(j,1),(beta1_final(:,1)),(sqrt(sig_final(:,1)))) + ...
   (1- (P_final(:,1))).*normpdf(ygrid(j,1),(beta1_final(:,2)),(sqrt(sig_final(:,2)))) );
end;
plot(ygrid,density);
true_density = Puse*normpdf(ygrid,m1,sqrt(s1)) + (1-Puse)*normpdf(ygrid,m2,sqrt(s2));

hold on;
plot(ygrid,true_density,'--');
hold off;
clc;
disp('Mean of intercepts');
mean(beta1_final)
disp('Mean of Variance Parameters');
mean(sig_final)
disp('Mean of Probability of first component');
mean(P_final)
save bimod_mix_res ygrid density true_density;