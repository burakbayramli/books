clear;
clc;
clf;
randn('seed',sum(100*clock));

load nonparm_data;
%-------------
%SORT THE DATA
%-------------
full_data = [x y];
nobs = length(y);
data_sorted = sortrows(full_data,1);
clear x y;
x = data_sorted(:,1);
y = data_sorted(:,2);
diff_x = (x(2:nobs) - x(1:nobs-1)).^(-1);

%create the differencing matrix H
H = zeros(nobs,nobs);
H(1,:) = [1 zeros(1,nobs-1)];
H(2,:) = [0 1 zeros(1,nobs-2)];
for j = 3:nobs;
    H(j,:) = [zeros(1,j-3) diff_x(j-2) (-(diff_x(j-1) + diff_x(j-2))) diff_x(j-1) zeros(1,nobs-j)];
end;
invH = inv(H);
invHH = invH'*invH;


%create the prior covariance matrix
eta = .00001;                          %CHANGE THIS LINE TO CHANGE THE VALUE OF THE SMOOTHING PARAMETER!!
mu_1 = zeros(2,1);                  %prior mean for initial conditions
mu = [mu_1; zeros(nobs-2,1)];       %prior mean for all parameters
V_1_inv = (1/100)*eye(2);            %inv. covmat for initial conditions
V_2_inv = (1/eta)*eye(nobs-2);      %inv covmat for difference in pointwise slopes
V_inv = blockdiag(V_1_inv,V_2_inv); %full covmat

%priors
a = 3;
b = (1/(2*.1));

iter = 250;
burn = 50;


sigeps = .1;
gammas_final = zeros(iter-burn,nobs);
sigeps_final = zeros(iter-burn,1);

for i = 1:iter;
       %-------------
       %sample reg. parms
       %-----------------
       D_gamma = inv(invHH/sigeps + V_inv);
       d_gamma = invH'*y/sigeps + V_inv*mu;
       H_gamma = chol(D_gamma);
       gammas = D_gamma*d_gamma + H_gamma'*randn(nobs,1);
       thetas = invH*gammas;
       %-------------------
       %sample var parameter
       %--------------------
       resids = y - invH*gammas;
       sigeps = invgamrnd( (nobs/2) + a, inv( inv(b) + .5*resids'*resids),1,1);
       
       if i > burn;
           thetas_final(i-burn,:) = thetas';
           sigeps_final(i-burn,1) = sigeps;
       end;
   end;
           

   fun_est = mean(thetas_final)';
xgrid = linspace(min(x),max(x),200);
ytrue = .15*xgrid + .3*exp(-4*((xgrid+1).^2)) + .7*exp( -16*((xgrid-1).^2) );
plot(x,y,'.');
hold on;
plot(xgrid,ytrue,'r');
plot(x,fun_est,'--b');
save resultssmall fun_est ytrue x y xgrid;
