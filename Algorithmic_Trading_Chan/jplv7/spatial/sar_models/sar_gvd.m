% PURPOSE: An example of using sar_gv() Gibbs sampling
%          spatial autoregressive model that constructs
%          a posterior distribution for the heteroscedasticity
%          parameter r (rather than use a degenerate prior on r)
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: sar_gvd 
%---------------------------------------------------

clear all;

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
n = length(y); 
x = [ones(n,1) x1 x2 x3];

n = 3107;
[junk W junk] = xy2cont(latt,long);
vnames = strvcat('voters','const','educ','homeowners','income');


ndraw = 2500;
nomit = 500;
prior.delta = 20; % homoscedastic prior
results = sar_gv(y,x,W,ndraw,nomit,prior);
prt(results,vnames);

% construct posterior distribution for r-value from
% sampled values to draw inference about heteroscedasticity

subplot(2,1,1),
histo(results.rdraw);
subplot(2,1,2),
plot(results.acc);
title('acceptance rate for M-H sampling');
fprintf(1,'prior mean for r     = %16.8f \n',results.delta);
fprintf(1,'posterior mean for r = %16.8f \n',mean(results.rdraw));
fprintf(1,'posterior std for r  = %16.8f \n',std(results.rdraw));
pause;

prior2.delta = 4;  % heteroscedastic prior 
results2 = sar_gv(y,x,W,ndraw,nomit,prior2);
prt(results2,vnames);
subplot(2,1,1),
histo(results2.rdraw);
subplot(2,1,2),
plot(results2.acc);
title('acceptance rate for M-H sampling');
fprintf(1,'prior mean for r     = %16.8f \n',results2.delta);
fprintf(1,'posterior mean for r = %16.8f \n',mean(results2.rdraw));
fprintf(1,'posterior std for r  = %16.8f \n',std(results2.rdraw));
pause;


% do a plot of posteriors for r-value
[h1,f1,y1] = pltdens(results.rdraw);
[h2,f2,y2] = pltdens(results2.rdraw);

subplot(1,1,1);
plot(y1,f1,'.r',y2,f2,'.g');
legend('r-value posterior homo prior','r-value posterior hetero prior');
title('r-parameter posterior distributions');


