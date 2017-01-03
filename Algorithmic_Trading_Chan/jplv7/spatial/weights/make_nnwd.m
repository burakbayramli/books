% PURPOSE: An example of using make_nnw
%          a nearest neighbor spatial weight matrix
%          on a small data set                   
%---------------------------------------------------
% USAGE: make_nnwd
%---------------------------------------------------

clear all;

% load Anselin (1988) Columbus neighborhood crime data
load anselin.dat; 
xc = anselin(:,4);
yc = anselin(:,5);
% To construct a row-stochastic weight matrix based on m neighbors
% (where m is the # of nearest neighbors,)

W2 = make_nnw(xc,yc,2); 

% 4 neighbors
W4 = make_nnw(xc,yc,4);

spyc(W2,'o');
hold on;
spyc(W4,'+');
title('2 versus 4 nearest neighbors W-matrices');
legend('2 nearest','4 nearest');
