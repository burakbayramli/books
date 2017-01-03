% PURPOSE: An example of using gwr_logit()
%          Geographically weighted logit regression model
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: gwr_d 
%---------------------------------------------------

clear all;
% load the Anselin data set
load anselin.dat;
y = anselin(:,1);
nobs = length(y);
x = [ones(nobs,1) anselin(:,2:3)];
[nobs nvar] = size(x);
north = anselin(:,4);
east = anselin(:,5);

% convert y's to 0,1 values
ymean = mean(y);
yzip = zeros(nobs,1);
ind = find(y > ymean);
yzip(ind,1) = 1;

vnames = strvcat('crime','constant','income','hvalue');

% y =  dependent variable
% x = a matrix of indepdendent variables
% east holds  x-coordinates
% north holds y-coordinates
% nobs = # of observations
% nvar = # of explanatory variables
info.dtype = 'gaussian'; % Gaussian distance weighting
tic; result1 = gwr_logit(yzip,x,east,north,info); toc;
prt(result1,vnames);

tt=1:nobs;
plot(tt,yzip,'+',tt,result1.yhat,'o');
title('actual vs predicted');