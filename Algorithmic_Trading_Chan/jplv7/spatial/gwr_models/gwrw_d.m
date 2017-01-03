% PURPOSE: An example of using gwrw()
%          to produce a weight matrix for GWR model
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: gwrw_d 
%---------------------------------------------------

% load the Anselin data set
load anselin.dat;
y = anselin(:,1);
nobs = length(y);
x = [ones(nobs,1) anselin(:,2:3)];
[nobs nvar] = size(x);
north = anselin(:,4);
east = anselin(:,5);

wmat1 = gwrw(east,north,0.5,'gaussian');
contour(wmat1);
pause;

wmat2 = gwrw(east,north,10,'exponential');
contour(wmat2);
pause;

wmat3 = gwrw(east,north,15,'tricube');
contour(wmat3);

