% PURPOSE: An example of using sarp_g() on a large data set   
%          Gibbs sampling spatial autoregressive probit model                         
%---------------------------------------------------
% USAGE: sarp_gd2 (see sarp_gd for a small data set)
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
W = make_neighborsw(latt,long,6);
vnames = strvcat('voters','const','educ','homeowners','income');

rho = 0.7;
beta = ones(k,1);
beta(1:2,1) = -1;

y = (speye(n) - rho*W)\(x*beta) + (speye(n) - rho*W)\randn(n,1);
ysave = y;

ndraw = 1200; 
nomit = 200;

prior.novi = 1;
result = sar_g(ysave,x,W,ndraw,nomit,prior);
prt(result,vnames);

prior2.nstep = 1;
y = (y > 0)*1.0; % convert to 0,1 y-values
result2 = sarp_g(y,x,W,ndraw,nomit,prior2);
prt(result2,vnames);


% plot densities for comparison
[h1,f1,y1] = pltdens(result.bdraw(:,1));
[h2,f2,y2] = pltdens(result2.bdraw(:,1));
[h3,f3,y3] = pltdens(result.bdraw(:,2));
[h4,f4,y4] = pltdens(result2.bdraw(:,2));
[h5,f5,y5] = pltdens(result.bdraw(:,3));
[h6,f6,y6] = pltdens(result2.bdraw(:,3));

plot(y1,f1,'.r',y2,f2,'.g');
legend('sar','sarp');
xlabel(['true b =' num2str(beta(1,1))]);
pause;
plot(y3,f3,'.r',y4,f4,'.g');
legend('sar','sarp');
xlabel(['true b =' num2str(beta(2,1))]);
pause;
plot(y5,f5,'.r',y6,f6,'.g');
legend('sar','sarp');
xlabel(['true b =' num2str(beta(3,1))]);
pause;


[h5,f5,y5] = pltdens(result.pdraw);
[h6,f6,y6] = pltdens(result2.pdraw);

plot(y5,f5,'.r',y6,f6,'.g');
legend('sar','sarp');
xlabel(['true rho =' num2str(rho)]);
pause;
