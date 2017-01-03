% PURPOSE: A Monte Carlo comparison of sem_gmm and ML sem
% GM estimation of the spatial error model
% in a Monte Carlo experiment that compares maximum likelihood
% and GM estimation  
%---------------------------------------------------
% USAGE: sem_gmmd3 (see also sem_gmmd2 for a large data set)
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
rho = 0.85;  % true value of rho
sige = 0.5;
k = 3;
x = randn(n,k);
beta = zeros(k,1);
beta(1,1) = 1.0;
beta(2,1) = -1.0;
beta(3,1) = 1.0;

niter = 100;
bout = zeros(niter,2*k); % storage for results
pout = zeros(niter,2);
sout = zeros(niter,2);

for iter=1:niter;

u = (IN - rho*W)\(randn(n,1)*sqrt(sige));
y = x*beta + u;

if iter == 1
results0 = sem(y,x,W);
lndetv = results0.lndet;
else
options.lndet = lndetv;
results0 = sem(y,x,W,options);
end;

bout(iter,1:k) = results0.beta';
sout(iter,1) = results0.sige;
pout(iter,1) = results0.rho;

bout(iter,1:k) = results0.beta';
sout(iter,1) = results0.sige;
pout(iter,1) = results0.rho;

results1 = sem_gmm(y,x,W);
bout(iter,k+1:2*k) = results1.beta';
sout(iter,2) = results1.sige;
pout(iter,2) = results1.lambda;

end;

% pretty print out the results

in.cnames = strvcat('ML b1','ML b2','ML b3','GM b1','GM b2','GM b3');
in.rnames = strvcat('Statistics','means','median','min','max');
in.width = 1000;

bprint = [mean(bout)
          median(bout)
          min(bout)
          max(bout)];

mprint(bprint,in);

pprint = [mean(pout)   mean(sout)
          median(pout) median(sout)
          min(pout)    min(sout)
          max(pout)    max(sout)];

in2.cnames = strvcat('ML lambda','GM lambda','ML sigma','GM sigma');
in2.rnames = strvcat('Statistics','means','median','min','max');
in2.width = 1000;

fprintf(1,'results from 100 Monte Carlo simulations \n');
mprint(pprint,in2);



