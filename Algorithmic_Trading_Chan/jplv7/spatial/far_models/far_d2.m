% PURPOSE: An example of using far_g()
%          1st order spatial autoregressive model
%          on a large dataset                    
%---------------------------------------------------
% USAGE: far_gd2, see far_gd for a small dataset example
%---------------------------------------------------

% NOTE a large data set with 3107 observations
% from Pace and Barry, takes around 150 seconds

clear all;
load elect.dat;             % load data on votes
y = log(elect(:,7)./elect(:,8)); % proportion of voters casting votes
ydev = y - mean(y);         % deviations from the means form 
latt = elect(:,5);
long = elect(:,6);
clear elect;                % conserve on RAM memory
n = 3107;
[junk W junk] = xy2cont(latt,long);

% do maximum likelihood for comparison
info.lflag = 1; % use Pace and Barry MC approximation to ln det
result1 = far(ydev,W,info);
prt(result1);
plt(result1);