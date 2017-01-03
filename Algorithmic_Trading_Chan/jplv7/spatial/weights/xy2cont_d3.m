% PURPOSE: An example of using xy2cont()
%          to produce a sparse spatial weight matrix 
%          using a VERY large data set                   
%---------------------------------------------------
% USAGE: xy2cont_d3
%---------------------------------------------------

clear all;
% number of observations
n=100000;

% generate artificial locational coordinates
xcoord = sort(randn(n,1));
ycoord = sort(randn(n,1));

% create contiguity matrix (1st order)
disp('time taken to produce W-matrix, 100,000 observations');
disp('patience ...');
tic;
[junk W junk] = xy2cont(xcoord,ycoord);
toc;

spy(W);
title('row-stochastic weight matrix 100,000 observations');
