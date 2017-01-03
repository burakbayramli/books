% PURPOSE: demo of binomial distribution functions
%          prints mean and variance of 1000 draws
%          plots pdf,cdf,inverse
% 
%---------------------------------------------------
% USAGE: bino_d
%---------------------------------------------------

nobs = 1000;
ntrials = 10;
psuccess = 0.5;

tic;
x = bino_rnd(ntrials,psuccess,nobs,1);
toc;


% mean should equal ntrials*psuccess
fprintf('mean should = %16.8f \n',ntrials*psuccess);
fprintf('mean of draws = %16.8f \n',mean(x));

ps = psuccess;

% variance should equal ntrials*psuccess*(1-psuccess)
fprintf('variance should = %16.8f \n',ntrials*ps*(1-ps));
fprintf('variance of draws = %16.8f \n',std(x)*std(x));

cdf = bino_cdf(sort(x),ntrials,psuccess);

plot(sort(x),cdf);
title('cdf for 10 with prob success = 0.5');
pause;

pdf = bino_pdf(sort(x),ntrials,psuccess);
plot(sort(x),pdf);
title('pdf for 10 with prob success = 0.5');
pause;
