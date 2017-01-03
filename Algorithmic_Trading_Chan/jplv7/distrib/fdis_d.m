% PURPOSE: demo of F-distribution functions
%          prints mean and variance of 1000 draws
%          plots pdf,cdf,inverse
% 
%---------------------------------------------------
% USAGE: fdis_d
%---------------------------------------------------

n = 1000;
a = 10;
b = 100;

tic;
tst = fdis_rnd(n,a,b);
toc;


% mean should equal (b/a)*((a/2)/(b/2-1))
fprintf('mean should   = %16.8f \n',(b/a)*((a/2)/(b/2-1)));
fprintf('mean of draws = %16.8f \n',mean(tst));

fmean = (b/a)*((a/2)/(b/2-1));

% variance should equal term1*(term2*term3)
% where:
 term1 = (b/a)*(b/a);
 tmp1 = a/2; 
 tmp2 = (b/2-1)*(b/2-1); 
 tmp3 = (a+b)/2;
 tmp4 = (b/2-2);
 term2 = tmp1*(tmp3 - 1);
 term3 =1/(tmp2*tmp4);
fprintf('variance should   = %16.8f \n',term1*(term2*term3));
fprintf('variance of draws = %16.8f \n',std(tst)*std(tst));

tst = unif_rnd(n,0.1,5);
tsort = sort(tst);

pdf = fdis_pdf(tsort,a,b);

plot(tsort,pdf);
title(['F pdf dof =',num2str(a) ',' num2str(b), ' mean=',num2str(fmean)]);
pause;

cdf = fdis_cdf(tsort,a,b);
plot(tsort,cdf);
title(['F cdf dof =',num2str(a) ',' num2str(b), ' mean=',num2str(fmean)]);
pause;

tst = rand(n,1);
tsort = sort(tst);

x = fdis_inv(tsort,a,b);
plot(tsort,x);
title(['F inverse dof =',num2str(a) ',' num2str(b), ' mean=',num2str(fmean)]);



