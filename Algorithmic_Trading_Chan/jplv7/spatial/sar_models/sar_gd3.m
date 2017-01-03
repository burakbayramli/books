% PURPOSE: An example of using sar_g() on a large data set   
%          Metropolis-Hastings sampling spatial autoregressive model                         
%---------------------------------------------------
% USAGE: sar_gd3 (see sar_gd for a small data set)
%---------------------------------------------------

clear all;
% NOTE a large data set with 3107 observations
% from Pace and Barry, takes around 150-250 seconds
load elect.dat;             % load data on votes
y =  log(elect(:,7)./elect(:,8));
x1 = log(elect(:,9)./elect(:,8));
x2 = log(elect(:,10)./elect(:,8));
x3 = log(elect(:,11)./elect(:,8));
latt = elect(:,5);
long = elect(:,6);
n = length(y); x = [ones(n,1) x1 x2 x3];
clear x1; clear x2; clear x3;
clear elect;                % conserve on RAM memory
n = 3107;
[junk W junk] = xy2cont(latt,long);
vnames = strvcat('voters','const','educ','homeowners','income');

% do Gibbs sampling estimation
ndraw = 2500; 
nomit = 500;
prior.novi = 1;  % homoscedastic model
result1 = sar_g(y,x,W,ndraw,nomit,prior);
prt(result1,vnames);

prior2.rval = 4;  % heteroscedastic model
result2 = sar_g(y,x,W,ndraw,nomit,prior2);
prt(result2,vnames);

% compare the posterior densities for rho
[h1,f1,y1] = pltdens(result1.pdraw);
[h2,f2,y2] = pltdens(result2.pdraw);

plot(y1,f1,'--r',y2,f2,'-g');
legend('Homoscedastic','heteroscedastic');
pause;

[h1,f1,y1] = pltdens(result1.yhat);
[h2,f2,y2] = pltdens(result2.yhat);
[h3,f3,y3] = pltdens(y);

plot(y1,f1,'--r',y2,f2,'-g',y3,f3,'.b');
legend('Homoscedastic','heteroscedastic','actual');
axis([-2 1 0 max(max([f1 f2 f3]))]);
pause;

tt=1:n;

plot(tt,y,'.b',tt,result1.yhat,'.r');
title('actual vs predicted homoscedastic model');
xlabel('actual');
ylabel('predicted');
pause;

plot(tt,y,'.b',tt,result2.yhat,'.r');
title('actual vs predicted heteroscdastic model');
xlabel('actual');
ylabel('predicted');





