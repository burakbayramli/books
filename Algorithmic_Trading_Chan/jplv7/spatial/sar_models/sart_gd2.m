% PURPOSE: An example of using sart_g() on a large data set   
%          Gibbs sampling spatial autoregressive tobit model                         
%---------------------------------------------------
% USAGE: sart_gd2 (see sart_gd for a small data set)
%---------------------------------------------------

clear all;
% NOTE a large data set with 3107 observations
% from Pace and Barry, 
load elect.dat;             % load data on votes
latt = elect(:,5);
long = elect(:,6);
n = length(latt);
k = 4;
x = randn(n,k);
clear elect;                % conserve on RAM memory
n = 3107;
[junk W junk] = xy2cont(latt,long);
vnames = strvcat('voters','const','educ','homeowners','income');

rho = 0.7;
beta = ones(k,1);
sige = 1;

y = (speye(n) - rho*W)\(x*beta) + (speye(n) - rho*W)\randn(n,1)*sqrt(sige);
limit = mean(y);
ysave = y;
ind = find(y < limit);
y(ind,1) = limit; % censored  values

% do Gibbs sampling estimation
ndraw = 2500; 
nomit = 500;
%prior.rval = 4;
prior.novi = 1;

result = sar_g(ysave,x,W,ndraw,nomit,prior); % MCMC estimates based on
prt(result);                                  % non-truncated data for comparison

prior2.limit = limit;
prior2.trunc = 'left';
prior2.novi = 1;
result2 = sart_g(y,x,W,ndraw,nomit,prior2); % tobit estimates
prt(result2,vnames);

tind = find(ysave < 0);
ytrunc = ysave(tind,1);
ydraws = result2.ymean(tind,1);
tt=1:length(ydraws);
plot(tt,ytrunc,'.r',tt,ydraws,'og');
title('actual y vs mean of latent y-draws');
xlabel('truncated observations');
legend('actual','latent y-draws');
pause;

[h1,f1,y1] = pltdens(ytrunc);
[h2,f2,y2] = pltdens(ydraws);
plot(y1,f1,'-r',y2,f2,'--b');
title('density of actual truncated values vs posterior mean of y-draws');
pause;

% plot densities for comparison
[h1,f1,y1] = pltdens(result.bdraw(:,1));
[h2,f2,y2] = pltdens(result2.bdraw(:,1));
[h3,f3,y3] = pltdens(result.bdraw(:,2));
[h4,f4,y4] = pltdens(result2.bdraw(:,2));
[h5,f5,y5] = pltdens(result.bdraw(:,3));
[h6,f6,y6] = pltdens(result2.bdraw(:,3));

plot(y1,f1,'.r',y2,f2,'.g');
legend('sar','sart');
xlabel(['true b =' num2str(beta(1,1))]);
pause;
plot(y3,f3,'.r',y4,f4,'.g');
legend('sar','sart');
xlabel(['true b =' num2str(beta(2,1))]);
pause;
plot(y5,f5,'.r',y6,f6,'.g');
legend('sar','sart');
xlabel(['true b =' num2str(beta(3,1))]);
pause;


[h5,f5,y5] = pltdens(result.pdraw);
[h6,f6,y6] = pltdens(result2.pdraw);

plot(y5,f5,'.r',y6,f6,'.g');
legend('sar','sart');
xlabel(['true rho =' num2str(rho)]);



