% PURPOSE: An example of using sar on a large data set   
%          Gibbs sampling spatial autoregressive model                         
%---------------------------------------------------
% USAGE: sar_d3 (see sar_d for a small data set)
%---------------------------------------------------

clear all;
n = 3107;

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
[junk W junk] = xy2cont(latt,long);
vnames = strvcat('voters','const','educ','homeowners','income');

info.lflag = 0;           % use full lndet calculation
result = sar(y,x,W,info); % maximum likelihood estimates
prt(result,vnames);

                      % use default MC approximation for lndet calculation
result2 = sar(y,x,W); % maximum likelihood estimates
prt(result2,vnames);
