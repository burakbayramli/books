% PURPOSE: An example of using walds()
%          test for spatial autocorrelation
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: walds_d
%---------------------------------------------------

% PURPOSE: demonstrates lmerror routine
clear all;

load anselin.dat;
y = anselin(:,1);
x = [ones(49,1) anselin(:,2:3)];

xc = anselin(:,4);
yc = anselin(:,5);
[j W j] = xy2cont(xc,yc);

result = walds(y,x,W);
prt(result);

