% PURPOSE: A timing comparison of heteroscedastic versus homoscedastic on small and large datasets                    
%---------------------------------------------------
% USAGE: far_timing
%---------------------------------------------------

clear all;
load anselin.dat; % small 49-observation dataset
xc = anselin(:,4);
yc = anselin(:,5);
y = anselin(:,1);
n = length(y);
x = [ones(n,1) anselin(:,2:3)];
[j1 W j2] = xy2cont(xc,yc);

times = zeros(2,2);

ndraw = 2000;
nomit = 500;
prior.rval = 4; % heteroscedastic prior
result = sem_g(y,x,W,ndraw,nomit,prior); 
times(1,1) = result.time; % matlab version

prior.novi = 1; % homoscedastic prior
result = sem_g(y,x,W,ndraw,nomit,prior); 
times(1,2) = result.time; % matlab version

% NOTE a large data set with 3107 observations
% from Pace and Barry

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

prior.rval = 4; % heteroscedastic prior
result = sem_g(y,x,W,ndraw,nomit,prior);
times(2,1) = result.time;

prior.novi = 1;
result = sem_g(y,x,W,ndraw,nomit,prior);
times(2,2) = result.time;

in.cnames = strvcat('heteroscedastic','homoscedastic');
in.rnames = strvcat('time in seconds','49 observations','3,107 observations');
fprintf('time for 2,000 draws \n');
mprint(times,in);

