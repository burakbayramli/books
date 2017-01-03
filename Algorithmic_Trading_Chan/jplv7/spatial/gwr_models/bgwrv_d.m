% PURPOSE: An example of using bgwr()
%          Geographically weighted regression model
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: bgwr_d
%---------------------------------------------------

% load the Anselin data set
load anselin.dat;

y = anselin(:,1);
nobs = length(y);
x = [ones(nobs,1) anselin(:,2:3)];
[junk nvar] = size(x);
east = anselin(:,4);
north = anselin(:,5);
ndraw = 550; nomit = 50;
prior.dtype = 'exponential';
prior.rval = 4;
result = bgwrv(y,x,east,north,ndraw,nomit,prior);

vnames = strvcat('crime','constant','income','hvalue');
prt(result,vnames);

% compare gwr estimates with posterior means
b1 = result.bdraw(:,:,1);
b2 = result.bdraw(:,:,2);
b3 = result.bdraw(:,:,3);

b1mean = mean(b1);
b2mean = mean(b2);
b3mean = mean(b3);

info2.dtype = 'exponential';
result2 = gwr(y,x,east,north,info2);
bgwr = result2.beta;

tt=1:nobs;
plot(tt,bgwr(:,1),'-r',tt,b1mean,'--b');
legend('gwr','bgwr');
title('b1 parameter');
pause;

plot(tt,bgwr(:,2),'-r',tt,b2mean,'--b');
legend('gwr','bgwr');
title('b2 parameter');
pause;

plot(tt,bgwr(:,3),'-r',tt,b3mean,'--b');
legend('gwr','bgwr');
title('b3 parameter');
pause;
      
% plot mean of vi draws across observations
plot(tt,result.vmean);
title('vi means');
pause;


