% PURPOSE: An example of using lratios()
%          to test for spatial error correlation
%          on a small data set                   
%---------------------------------------------------
% USAGE: lratios_d
%---------------------------------------------------


load anselin.dat;

y = anselin(:,1);
n = length(y);

x = [ones(n,1) anselin(:,2:3)];

load wmat.dat;
W = sparse(wmat(:,1),wmat(:,2),wmat(:,3));


res = sem(y,x,W);
result = lratios(y,x,W,res);
prt(result);


