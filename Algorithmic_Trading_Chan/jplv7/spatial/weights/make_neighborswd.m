% PURPOSE: An example of using make_neighborsw
%          a nearest neighbor spatial weight matrix
%          on a small data set                   
%---------------------------------------------------
% USAGE: make_neighborsw
%---------------------------------------------------

clear all;

% load Anselin (1988) Columbus neighborhood crime data
load anselin.dat; 
xc = anselin(:,4);
yc = anselin(:,5);
% To construct a row-stochastic weight matrix based on m neighbors
% (where m is the # of nearest neighbors,)
% W2 = make_nnw(xc,yc,2);
% W4 = make_nnw(xc,yc,4);


W2 = make_neighborsw(xc,yc,2);
W4 = make_neighborsw(xc,yc,4);


% now show the spatial weight matrices
spyc(W2,'.r',30);
hold on;
spyc(W4,'og',10);
title('2 and 4 nearest neighbors W-matrices');
legend('2 nearest','4 nearest');
hold off;

fprintf(1,'non-symmetric row-stochastic weight matrix \n');
fprintf(1,'sum of 1st row elements = %8.2f \n',sum(full(W4(1,:))));
fprintf(1,'sum of 1st column elements = %8.2f \n',sum(full(W4(:,1))));

