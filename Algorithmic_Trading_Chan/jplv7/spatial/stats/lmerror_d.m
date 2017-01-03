% PURPOSE: An example of using lmerror()
%          to test for spatial error correlation
%          on a small data set                   
%---------------------------------------------------
% USAGE: lmerror_d
%---------------------------------------------------

% PURPOSE: demonstrates lmerror routine

load anselin.dat;
y = anselin(:,1);
x = [ones(49,1) anselin(:,2:3)];

load wmat.dat; 
load wmat.dat;
W = sparse(wmat(:,1),wmat(:,2),wmat(:,3));


result = lmerror(y,x,W);
prt(result);

