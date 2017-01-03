% PURPOSE: An example of using darp
%          Casetti's DARP model
%                              
%---------------------------------------------------
% USAGE: darp_d
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

% do Casetti darp x-y expansion
tic;
res1 = darp(y,x,xc,yc);
toc;
% print the output
prt(res1,vnames);
plt(res1,vnames);
pause;

% do Casetti darp using distance expansion
% from a central observation #32
option.exp = 1;
option.ctr = 32;
tic;
res2 = darp(y,x,xc,yc,option);
toc;
% print the output
prt(res2,vnames);
plt(res2,vnames);
%pause;

