% PURPOSE: An example of using sac() on a small dataset
%          general spatial model
%                              
%---------------------------------------------------
% USAGE: sac_d
%---------------------------------------------------

clear all;


% load Anselin (1988) Columbus neighborhood crime data
load anselin.dat; 
% 5 columns:
% column1 = crime
% column2 = household income
% column3 = house values
% column4 = latitude coordinate
% column5 = longitude coordinate

n = length(anselin);
y = anselin(:,1);
x = [ones(n,1) anselin(:,2:3)]; 
latt = anselin(:,4);
long = anselin(:,5);
vnames = strvcat('crime','constant','income','hvalue');

% 1st order contiguity matrix for
% Anselin's Columbus crime dataset
% stored in sparse matrix format [i, j, s] = find(W);
% so that W = sparse(i,j,s); reconstructs the 49x49 matrix
% NOTE: already row-standardized

load wmat.dat;
W = sparse(wmat(:,1),wmat(:,2),wmat(:,3));



result1 = sac(y,x,W,W);
% print the output with variable names
prt(result1,vnames);

result2 = sac_gmm(y,x,W,W);
prt(result2,vnames);


