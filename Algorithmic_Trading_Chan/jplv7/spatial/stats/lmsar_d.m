% PURPOSE: An example of using lmsar()
%          to test for spatial error correlation
%          on a small data set                   
%---------------------------------------------------
% USAGE: lmsar_d
%---------------------------------------------------

clear all;
load anselin.dat;
y = anselin(:,1);
x = [ones(49,1) anselin(:,2:3)];
xc = anselin(:,4);
yc = anselin(:,5);
[j1 W j2] = xy2cont(xc,yc);

result = lmsar(y,x,W,W);
prt(result);

