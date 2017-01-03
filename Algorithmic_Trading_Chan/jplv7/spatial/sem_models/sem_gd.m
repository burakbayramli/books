% PURPOSE: An example of using sem_g() 
% Gibbs sampling spatial error model(on a small data set)  
%                                   
%---------------------------------------------------
% USAGE: sem_gd (see also sem_gd2 for a large data set)
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
rho = 0.7;  % true value of rho
sige = 1;
k = 3;
x = randn(n,k);
beta(1,1) = 1.0;
beta(2,1) = -1.0;
beta(3,1) = 1.0;

% homoscedastic disturbances
u = (IN - rho*W)\(randn(n,1)*sqrt(sige));
y = x*beta + u;


ndraw = 2500;
nomit = 500;

% maximum likelihood estimates
info.lflag = 0; % don't use Pace-Barry lndet approximation
results0 = sem(y,x,W,info);
prt(results0);

prior.novi = 1;  % homoscedastic model for comparison with max lik
prior.lflag = 0; % don't use Pace-Barry lndet approximation

results1 = sem_g(y,x,W,ndraw,nomit,prior);
results1.tflag = 'tstat';
prt(results1);

prior2.rval = 200; % homoscedastic model for comparison with max lik
prior2.lflag = 0;  % don't use Pace-Barry lndet approximation

results2 = sem_g(y,x,W,ndraw,nomit,prior2);
results2.tflag = 'tstat';
prt(results2);

prior3.rval = 4;   % heteroscedastic model for comparison with max lik
prior3.lflag = 0;  % don't use Pace-Barry lndet approximation

results3 = sem_g(y,x,W,ndraw,nomit,prior3);
results3.tflag = 'tstat';
prt(results3);


% construct kernel density estimates for rho posteriors
[h1,f1,y1] = pltdens(results1.pdraw);
[h2,f2,y2] = pltdens(results2.pdraw);
[h3,f3,y3] = pltdens(results3.pdraw);

plot(y1,f1,'.r',y2,f2,'.g',y3,f3,'.b');
legend('novi','rval=200','rval=4');
title('rho posterior distributions');

% print comparison of estimates
out = [results0.beta mean(results1.bdraw)' mean(results2.bdraw)' mean(results3.bdraw)' 
       results0.rho  mean(results1.pdraw)  mean(results2.pdraw)  mean(results3.pdraw) ];

in.cnames = strvcat('max lik','novi','rval=200','rval=4');
in.rnames = strvcat('coefficients','b0','b1','b2','lambda');

fprintf(1,'\n comparison of mean estimates \n');
mprint(out,in);

out2 = [results0.beta median(results1.bdraw)' median(results2.bdraw)' median(results3.bdraw)' 
        results0.rho  median(results1.pdraw)  median(results2.pdraw)  median(results3.pdraw)  ];

fprintf(1,'\n comparison of median estimates \n');
mprint(out2,in);


probs = model_probs(results1,results2,results3);

fprintf(1,'posterior probs for 3 sem models \n');
in2.rnames = strvcat('Models','novi','rval=200','rval=4');
mprint(probs,in2);



