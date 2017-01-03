% PURPOSE: An example of using bcasetti
%          Bayesian version of Casetti's spatial expansion model
%                              
%---------------------------------------------------
% USAGE: bcasetti_d
%---------------------------------------------------

% load Anselin (1988) Columbus neighborhood crime data
load anselin.dat;

y = anselin(:,1);
n = length(y);
x = [ones(n,1) anselin(:,2:3)];

% Anselin (1988) x-y coordinates
xc = anselin(:,4);
yc = anselin(:,5);

vnames = strvcat('crime','const','income','hse value');

% do Casetti regression using x-y expansion
res1 = casetti(y,x,xc,yc);

% print the output
prt(res1,vnames);
plt(res1,vnames);
pause;

% do Casetti regression using distance expansion
% from a central observation #20
option.exp = 1;
option.ctr = 20;
option.rval = 4;
ndraw = 550;
nomit = 50;
res2 = bcasetti(y,x,xc,yc,ndraw,nomit,option);

% print the output
prt(res2,vnames);
plt(res2,vnames);
pause;

