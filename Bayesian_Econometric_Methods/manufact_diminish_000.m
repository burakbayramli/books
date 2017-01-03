%This m-file runs a Gibbs sampler on the 
%manufacturing data, imposing diminishing 
%returns to scale

clear;
clc;
randn('seed',sum(100*clock));

load prod_function;
labor = prod_function(:,1);
capital = prod_function(:,2);
output = prod_function(:,3);

y = log(output);
nobs = length(y)
X = [ones(nobs,1) (log(labor./capital)) log(capital)];

%-----------
%Priors
%-----------
V_beta = 10*eye(3);


burn = 5000;
iter = burn + 50*2000;
rts_final = zeros(iter-burn,1);

sigeps = 1;
[bhat stderr tstat sig rsq] = ols(X,y);
gamma_current = bhat;

index_vec = [1 2 3];
a = [-999 -999 -999]';
b = [999 999 1];
for i = 1:iter;
    %--------------
    %Draw gammas
    %--------------
        OMEGA = X'*X/sigeps + inv(V_beta);
        GAMMA_BAR = inv(OMEGA)*(X'*y/sigeps);
    for j = 1:3;
        whole_vec = OMEGA(j,:)'.*(gamma_current - GAMMA_BAR);
        points = find(index_vec~=j);
        vec_keep = whole_vec(points);
        mean_part = GAMMA_BAR(j) - inv(OMEGA(j,j))*sum(vec_keep);
        var_part = inv(OMEGA(j,j));
        gamma_draw = truncnorm2(mean_part,var_part,a(j),b(j));
        gamma_current(j) = gamma_draw;
    end;
        gamma_current
        
    
    %-----------
    %Draw sigma^2
    %------------
    resid = y - X*gamma_current;
    sigeps = invgamrnd( (nobs/2), inv(.5*resid'*resid),1,1);
    
    if i > burn;
        rts_final(i-burn,1) = gamma_current(3);
    end;
end;

num_keep = ((iter-burn)/50);
keep_rts = zeros(num_keep,1);
for j = 1:num_keep;
    tempp = rts_final(50*j,1);
    keep_rts(j,1) = tempp;
end;

save man_diminish keep_rts;
[dom ran] = epanech2(rts_final);
plot(dom,ran);
xlabel('Return to Scale');
ylabel('Density');
