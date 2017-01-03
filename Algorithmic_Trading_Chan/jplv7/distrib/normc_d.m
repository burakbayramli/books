% PURPOSE: demo of contaminated normal random numbers
%
%---------------------------------------------------
% USAGE: normc_d
%---------------------------------------------------

n = 10000;
mu = 0;
sig = 10;
gamm = 0.2;


tst = norm_crnd(n,gamm,sig) + mu;
tst2 = randn(n,1);

[h1 f1 y1] = pltdens(tst,0.5);
[h2 f2 y2] = pltdens(tst2,0.5);

y1t = find(y1 <= 5 & y1 >= -5);
y2t = find(y2 <= 5 & y2 >= -5);

plot(y1(y1t,1),f1(y1t,1),'+k',y2(y2t,1),f2(y2t,1),'--k');
legend('contaminated','normal');

