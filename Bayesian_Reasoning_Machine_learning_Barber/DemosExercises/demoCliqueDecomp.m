function demoCliqueDecomp
L=load('polbooks_links.txt'); % make the adjacency matrix A:
i=L(:,1); j=L(:,2); V=max(i); A=sparse(i,j,1,V,V); A=A+A'; A = (A + eye(V))>0;
opts.zloops = 15; opts.aloops=1; opts.plotprogress=1;
z=cliquedecomp(A,10,opts);