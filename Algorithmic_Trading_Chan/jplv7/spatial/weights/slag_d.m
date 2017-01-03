% PURPOSE: An example of using slag()
%          spatial lag function
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: slag_d
%---------------------------------------------------

clear all;
% load the Anselin data set 1st order contiguity matrix
load wmat.dat;
W = sparse(wmat(:,1),wmat(:,2),wmat(:,3));


W2 = slag(W,2);
W3 = slag(W,3);

spyc(W,'ok',20);
hold on;
spyc(W3,'+k',20);
hold off;
legend('W','W^3');
