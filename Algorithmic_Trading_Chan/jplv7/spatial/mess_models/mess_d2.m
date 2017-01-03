% PURPOSE: An example of using mess() on a large dataset
%          matrix exponential spatial specification
%---------------------------------------------------
% USAGE: mess_d2 (see mess_d for a small data set)
%---------------------------------------------------

% NOTE a large data set with 3107 observations
% from Pace and Barry, 
load elect.dat;             % load data on votes
y =  (elect(:,7)./elect(:,8));
x1 = log(elect(:,9)./elect(:,8));
x2 = log(elect(:,10)./elect(:,8));
x3 = log(elect(:,11)./elect(:,8));
n = length(y); x = [ones(n,1) x1 x2 x3];
clear x1; clear x2; clear x3;
latt = elect(:,5);
long = elect(:,6);

% a model based on 1st order contiguity matrix
[junk W junk] = xy2cont(latt,long);
option.D = W;
option.xflag = 0;
res1 = mess(y,x,option);
vnames = strvcat('voters','const','educ','homeowners','income');
prt(res1,vnames);

% a model with no spatially lagged x-variables
option2.latt = latt;
option2.long = long;
option2.neigh = 5;
option2.rho = 0.9;
res2 = mess(y,x,option2);
prt(res2,vnames);

% a model with spatially lagged x-variables
option3.latt = latt;
option3.long = long;
option3.neigh = 5;
option3.xflag = 1;
option3.rho = 1;
res3 = mess(y,x,option3);
prt(res3,vnames);




