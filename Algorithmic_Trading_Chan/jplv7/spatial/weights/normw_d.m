% PURPOSE: An example of using normw() to produce a normalized 1st order contiguity matrix
%          
%---------------------------------------------------
% USAGE: normw_d
%---------------------------------------------------

clear all;

n = 5;
W = sprandsym(n,.5);
[i,j,v]  = find(W~= 0);
W = sparse(i,j,v,n,n);

wout = normw(W);
disp('unormalized sparse matrix');
in.fmt = '%10.2f';
in.width = 100;
mprint(W,in);
disp('normalized sparse matrix');
mprint(wout,in);

W = sprandsym(n,.9);
[i,j,v]  = find(W~= 0);
W = sparse(i,j,v,n,n);

Wfull = full(W);
wout2 = normw(Wfull);

disp('unormalized full matrix');
in.fmt = '%10.2f';
in.width = 100;
mprint(W,in);
disp('normalized full matrix');
mprint(wout,in);

n = 2000;

W = sprandsym(n,.01);
[i,j,v]  = find(W~= 0);
W = sparse(i,j,v,n,n);

fprintf(1,'time taken to normalize a %d by %d matrix \n',n,n);
tic;
W = normw(W);
toc;
