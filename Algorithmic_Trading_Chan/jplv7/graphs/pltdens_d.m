% PURPOSE: demo of pltdens (non-parameter density plot)
%          
% 
%---------------------------------------------------
% USAGE: pltdens_d
%---------------------------------------------------

% generate normal density
n = 2000;
y = randn(n,1);

clf;
pltdens(y);
title('normal density, mean=0, var=1');
pause;

% plot 2 densities

y1 = randn(n,1)+2;
y2 = randn(n,1);

[h1 f1 x1] = pltdens(y1);
[h2 f2 x2] = pltdens(y2);

% force smoothness by doubling average of two
% default bandwidths
h = (h1+h2);

[h1 f1 x1] = pltdens(y1,h);
[h2 f2 x2] = pltdens(y2,h);


plot(x1,f1,x2,f2,'--');
title('two normal densities estimated');
legend('mean=2,var=1','mean=0,var=1');
pause;


% generate left-truncated normal
yin = zeros(n,1);
yout = normlt_rnd(yin,1,0);

% force smoothness by doubling default bandwidth
h = 2*(1.06*std(yout)*n^(-1/5)); % see help pltdens for default

clf;
pltdens(yout,0.5,1,1);
title('normal 0,1 left-truncated at 0');
