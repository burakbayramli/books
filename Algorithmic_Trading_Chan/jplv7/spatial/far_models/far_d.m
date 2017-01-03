% PURPOSE: An example of using far() on a small data set
%          1st-order spatial autoregressive model
%---------------------------------------------------
% USAGE: far_d (see also far_d2 for a large data set)
%---------------------------------------------------

% load Anselin (1988) Columbus neighborhood crime data
load anselin.dat;
% y =  dependent variable
% x = a matrix of indepdendent variables

y = anselin(:,1);
ydev = studentize(y); % standardized form in deviations from means
n = length(y);

% create 1st-order contiguity matrix
xc = anselin(:,4);
yc = anselin(:,5);
[j1 W j2] = xy2cont(xc,yc);

vnames = strvcat('crime','crime slag');

info.lflag = 0; % no approximation
res1 = far(ydev,W);
prt(res1,vnames);

info.lflag = 1; % MC approximation to lndet
res2 = far(ydev,W,info);
prt(res2,vnames);

info.lflag = 2; % spline approximation to lndet
res3 = far(ydev,W,info);
prt(res3,vnames);

plt(res3);