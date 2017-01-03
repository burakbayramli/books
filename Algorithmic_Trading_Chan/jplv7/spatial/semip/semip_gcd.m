% PURPOSE: An example of using semip_gc
% Gibbs sampling spatial Probit model with individual effects
% using 1996 presidential election data set
%                
%---------------------------------------------------
% USAGE: semip_gcd
%---------------------------------------------------

clear all;
load semip.mat;

vnames = strvcat('y','highs','college','grad','non-white', ...
'income','urban');

% x-matrix contains 3,110 x 6 matrix with:
%   col1 = high school graduates as a percent of population     
%   col2 = college percent  
%   col3 = graduate school 
%   col4 = non-white
%   col5 = median income     
%   col6 = urban  

% z = 0,1 with 0 = Dole wins, 1 = Clinton wins, 3,110 counties
% W = a 48x48 spatial weight matrix (standardized)
% nregions = 48
% regionobs = a 48 x 1 vector with the # of counties in each state
% states organized alphabetically

[n k] = size(x);


ndraw = 6000;
nomit = 1000;
prior.rval = 4;
prior.lflag = 0;
prior.rmin = -1;
prior.rmax = 1;

% matlab version 
tic;
result1 = semip_g(z,x,W,nregions,regionobs,ndraw,nomit,prior);
toc;
prt(result1,vnames);

% c-language mex file version 
tic;
result = semip_gc(z,x,W,nregions,regionobs,ndraw,nomit,prior);
toc;
prt(result,vnames);

yhat = result.yhat;
ymean = result.zmean;
vmean = result.vmean;


theta_std = std(result.adraw)';
theta_mean = mean(result.adraw)';
theta_hi = theta_mean + 3*theta_std;
theta_lo = theta_mean - 3*theta_std;


% sort the y percentages versus yhat
[ya yind] = sort(z);
ymeans = ymean(yind,1);
yhats = yhat(yind,1); 

tt=1:n;
plot(tt,ya,'+',tt,stdn_cdf(yhats),'o');
title('actual vs predicted');
pause;


tt=1:ndraw-nomit;
for ii=1:k;
plot(tt,result.bdraw(:,ii));
legend('c-estimates');
title(['beta draws for beta =  ' num2str(ii)]);
xlabel('in pause mode hit the return key');
pause;
end;

pltdens(result.pdraw);
title('posterior distribution of rho');
pause;

[wins wind] = sort(winner(:,1));

thetas = theta_mean(wind,1);
thetau = theta_hi(wind,1);
thetal = theta_lo(wind,1);


tt=1:nregions;
plot(tt,thetas,'-ok',tt,thetau,':k',tt,thetal,'--k');
%title('theta estimates with upper and lower 3-sigma bands');
legend('mean theta','upper 99','lower 99');
xlabel('States sorted by Dole - Clinton');
ylabel('Posterior mean of \theta values');
hold on;
plot(tt,zeros(nregions,1),'-k');
line([18 18],[-3 5]);
text(8,3,'Dole wins')
text(26,-2,'Clinton wins');
pause;

hold off;

vmeans = vmean(wind,1);

plot(tt,vmeans);
title('Vi-means sorted by theta values from low-to-high');
pause;
