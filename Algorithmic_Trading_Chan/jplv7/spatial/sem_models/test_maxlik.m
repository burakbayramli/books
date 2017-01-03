% PURPOSE: A test of the accuracy of max-like estimates
%          using Anselin's data set on Columbus crime               
%---------------------------------------------------
% USAGE: test_maxlik
%---------------------------------------------------

clear all;

load anselin.dat; % standardized 1st-order spatial weight matrix
y = anselin(:,1);
n = length(y);
x = [ones(n,1) anselin(:,2:3)];
[n,k] = size(x);
vnames = strvcat('crime','constant','income','hvalue');

load wmat.dat;
W = sparse(wmat(:,1),wmat(:,2),wmat(:,3));

% latt = anselin(:,4);
% long = anselin(:,5);
% [junk W junk] = xy2cont(latt,long);
[n junk] = size(W);



info.lflag = 0; % full lndet calculation
info.eigs = 0;  % compute eigenvalues
results3 = sem(y,x,W,info);
disp('compare to Table 12.6, page 195 in Anselin, 1988')
prt(results3,vnames);  
results4 = sdm(y,x,W,info);
disp('compare to Table 12.8, page 197 in Anselin, 1988')
prt(results4,vnames);  
results5 = sar(y,x,W,info);
disp('compare to Table 12.4, page 193 in Anselin, 1988')
prt(results5,vnames);

