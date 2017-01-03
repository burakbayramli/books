
clear all;
load elect.dat;                    % load data on votes
latt = elect(:,5);
long = elect(:,6);
n = length(latt);

W = make_neighborsw(latt,long,10);

x = [ones(n,1) randn(n,3)];

phi = 0.8;

x2 = [];
for i=1:3;
    xi = (speye(n) - phi*W)\randn(n,1);
    x2 = [x2 xi];
end;
    
xs = [ones(n,1) x2];  

beta = [10
        1
        -1
        1];

evec = randn(n,1);

rho = 0.65;
    
y = (speye(n) - rho*W)\(x*beta) + (speye(n) - rho*W)\evec;

y2 = (speye(n) - rho*W)\(xs*beta) + (speye(n) - rho*W)\evec;

vnames = strvcat('y','const','x1','x2','x3');

ndraw = 5500;
nomit = 1500;
prior.novi = 1;

result = sar_g(y,x,W,ndraw,nomit,prior); 
result.tflag = 'tstat';
prt(result,vnames);

resultx = sar_g(y2,xs,W,ndraw,nomit,prior); 
resultx.tflag = 'tstat';
prt(resultx,vnames);


info.ndraw = 4000;
result2 = sar_gmm(y,x,W,info);
prt(result2,vnames);

info.ndraw = 4000;
result2x = sar_gmm(y2,xs,W,info);
prt(result2x,vnames);

total_bayes = sum(squeeze(result.total(:,1,:)),2);
total_gmm = sum(squeeze(result2.total(:,1,:)),2);

total_bayesx = sum(squeeze(resultx.total(:,1,:)),2);
total_gmmx = sum(squeeze(result2x.total(:,1,:)),2);

[h1,f1,y1] = pltdens(total_bayes);
[h2,f2,y2] = pltdens(total_bayesx);
[h3,f3,y3] = pltdens(total_gmm);
[h4,f4,y4] = pltdens(total_gmmx);


plot(y1,f1,'-b',y2,f2,'-r',y3,f3,'-k',y4,f4,'-g');
legend('MCMC indep','MCMC depend','GMM/IV indep','GMM/VI depend');
title('total effects variable 1');
pause;

% ==== variable 3

total_bayes = sum(squeeze(result.total(:,3,:)),2);
total_gmm = sum(squeeze(result2.total(:,3,:)),2);

total_bayesx = sum(squeeze(resultx.total(:,3,:)),2);
total_gmmx = sum(squeeze(result2x.total(:,3,:)),2);

[h1,f1,y1] = pltdens(total_bayes);
[h2,f2,y2] = pltdens(total_bayesx);
[h3,f3,y3] = pltdens(total_gmm);
[h4,f4,y4] = pltdens(total_gmmx);


plot(y1,f1,'-b',y2,f2,'-r',y3,f3,'-k',y4,f4,'-g');
legend('MCMC indep','MCMC depend','GMM/IV indep','GMM/VI depend');
title('total effects variable 3');
pause;

% ==== variable 2

total_bayes = sum(squeeze(result.total(:,2,:)),2);
total_gmm = sum(squeeze(result2.total(:,2,:)),2);

total_bayesx = sum(squeeze(resultx.total(:,2,:)),2);
total_gmmx = sum(squeeze(result2x.total(:,2,:)),2);

[h1,f1,y1] = pltdens(total_bayes);
[h2,f2,y2] = pltdens(total_bayesx);
[h3,f3,y3] = pltdens(total_gmm);
[h4,f4,y4] = pltdens(total_gmmx);



plot(y1,f1,'-b',y2,f2,'-r',y3,f3,'-k',y4,f4,'-g');
legend('MCMC indep','MCMC depend','GMM/IV indep','GMM/VI depend');
title('total effects variable 2');
pause;

% ======= rho estimates


[h1,f1,y1] = pltdens(result.pdraw);
[h2,f2,y2] = pltdens(resultx.pdraw);

plot(y1,f1,'-b',y2,f2,'-r');
legend('rho posterior indep','rho posterior depend');
title('rho estimates MCMC');




