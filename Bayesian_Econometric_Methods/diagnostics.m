%calculate some diagnostics using generated data

clear;
clc;
randn('seed',sum(100*clock));
rand('seed',sum(100*clock));

beta0 = 1;
beta2 = .5;
beta_true = [1 .5]';
sigsq = .4;

nobs = 2000;
xmat = [ones(nobs,1) randn(nobs,1)];

%prior hyperparms
mu_beta = zeros(2,1);
V_beta = 100*eye(2);
a = 3;
b = (1/(2*.2));

iter = 2000;
burn = 100;
skew_final = zeros(iter-burn,3);
kurt_final = skew_final;
sigmasq = 1;


for j = 1:3;
%NOW, GENERATE Y. CHANGE EACH TIME

%normal errors
if j ==1;
y = xmat*beta_true + sqrt(sigsq)*randn(nobs,1);
disp('doing Normal');
elseif j==2;
    %UNIFORM ERRORS (here uniform from -5 to 5
        error_vec = -5 + 10*rand(nobs,1);
        y = xmat*beta_true + error_vec;
        disp('doing uniform');
    elseif j==3;
    %CHISQUARE ERRORS
        error_vec = chi2rnd(5,nobs,1) - 5;
        y = xmat*beta_true + error_vec;
        disp('doing chisquare')
    end;
    

for i = 1:iter
    
    %------------
    %sample beta
    %------------
    D_beta = inv(xmat'*xmat/sigmasq + inv(V_beta));
    d_beta = xmat'*y/sigmasq + inv(V_beta)*mu_beta;
    H_beta = chol(D_beta);
    betas = D_beta*d_beta + H_beta'*randn(2,1);
    
    %--------------
    %sample sigma^2
    %--------------
    sigmasq = invgamrnd((nobs/2) + a, inv( inv(b) + .5*(y - xmat*betas)'*(y - xmat*betas)),1,1);
    
    %calculate diagnostic measures - skewness and kurtosis
    resid = y - xmat*betas;
    skew = (mean( (resid.^3)))/( (sqrt(sigmasq))^(3/2) );
    kurt = (mean( (resid.^4)))/( ((sigmasq))^2 );
    
    if i > burn;
        skew_final(i-burn,j) = skew;
        kurt_final(i-burn,j) = kurt;
    end;
end;
    

end;

subplot(3,2,1);
[dom,ran] = epanech2(skew_final(:,1));
plot(dom,ran);
xlabel('Skewness - Normal');
%axis([-.4 1.6 0 6.5]);

subplot(3,2,3);
[dom ran] = epanech2(skew_final(:,2));
plot(dom,ran);
xlabel('Skewness - Uniform');
%axis([-.4 1.6 0 6.5]);

subplot(3,2,5);
[dom ran] = epanech2(skew_final(:,3));
plot(dom,ran);
xlabel('Skewness - Chisquare');
%axis([-.4 1.6 0 6.5]);


subplot(3,2,2);
[dom,ran] = epanech2(kurt_final(:,1));
plot(dom,ran);
xlabel('Kurtosis - Normal');


subplot(3,2,4);
[dom ran] = epanech2(kurt_final(:,2));
plot(dom,ran);
xlabel('Kurtosis - Uniform');


subplot(3,2,6);
[dom ran] = epanech2(kurt_final(:,3));
plot(dom,ran);
xlabel('Kurtosis - Chisquare');


