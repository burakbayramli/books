% PURPOSE: A Monte Carlo example of using sem_gmm
% GM estimation of the spatial error model
% in a Monte Carlo experiment  
%---------------------------------------------------
% USAGE: sem_gmmd4 (see also sem_gmmd2 for a large data set)
%---------------------------------------------------

clear all;

% W-matrix from Anselin's neigbhorhood crime data set
load anselin.dat; 
xc = anselin(:,4);
yc = anselin(:,5);
% crate standardized 1st-order spatial weight matrix
[j1 W j2] = xy2cont(xc,yc);
[n junk] = size(W);
IN = eye(n); 
rho = 0.8;  % true value of rho
sige = 5;
k = 3;
x = randn(n,k);
beta = zeros(k,1);
beta(1,1) = 1.0;
beta(2,1) = -1.0;
beta(3,1) = 1.0;

niter = 100;
bout = zeros(niter,k); % storage for results
pout = zeros(niter,1);
sout = zeros(niter,1);
rout = zeros(niter,1);

tic;
for iter=1:niter;

u = (IN - rho*W)\(randn(n,1)*sqrt(sige));
y = x*beta + u;

results1 = sem_gmm(y,x,W);
bout(iter,1:k) = results1.beta';
sout(iter,1) = results1.sige;
pout(iter,1) = results1.lambda;
rout(iter,1) = results1.rsqr;

end;
toc;
% pretty print out the results

in.cnames = strvcat('b1','b2','b3');
in.rnames = strvcat('Statistics','truth','means','median','min','max');
in.width = 1000;

bprint = [beta' 
          mean(bout)
          median(bout)
          min(bout)
          max(bout)];

mprint(bprint,in);

pprint = [rho          sige         0
          mean(pout)   mean(sout)    mean(rout)
          median(pout) median(sout)  median(rout)
          min(pout)    min(sout)     min(rout)
          max(pout)    max(sout)     max(rout)];

in2.cnames = strvcat('lambda','sigma','R-squared');
in2.rnames = strvcat('Statistics','truth','means','median','min','max');
in2.width = 1000;

mprint(pprint,in2);

ind = find(pout > 1);

plot(sort(pout),'.');



