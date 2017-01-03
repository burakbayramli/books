% PURPOSE: An example of using bgwr()
%          Geographically weighted regression model
%          (on a fairly large data set)                  
%---------------------------------------------------
% USAGE: bgwr_d2 
%---------------------------------------------------

clear all;
load boston.dat; % Harrison-Rubinfeld data
[n k] = size(boston);
y = boston(:,k-2);     % median house values
latt = boston(:,k-1);  % lattitude coordinates
long = boston(:,k);    % longitude coordinates

x = [ones(n,1) boston(:,1:k-3)];       % other variables
vnames = strvcat('hprice','crime','zoning','industry','charlesr', ...
         'noxsq','rooms2','houseage','distance','access','taxrate', ...
         'pupil/teacher','blackpop','lowclass');
ys = studentize(log(y)); xs = studentize(x(:,2:end));
clear boston; 
clear y;
clear x;
% NOTE: This baby takes 15 minutes on a fast computer.
%        If you have a slow computer forget it.
ndraw = 550; nomit = 50;
prior.ptype = 'distance'; % spatial parameter smoothing prior 
prior.rval = 4; % robustness prior for outliers
result = bgwr(ys,xs,latt,long,ndraw,nomit,prior);
prt(result,vnames);
plt(result,vnames);
