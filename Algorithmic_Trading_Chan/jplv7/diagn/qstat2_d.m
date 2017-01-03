% PURPOSE: demo of qstat2() Ljunbg-Box Q test
%          for AR(p) effects
% 
%---------------------------------------------------
% USAGE: qstat2_d
%---------------------------------------------------

% generate serially correlated time-series
nobs = 100; 
y = zeros(nobs,1);
y(1,1) = randn(1,1);
tt = 1:nobs;
tt = tt';
for i=2:nobs
y(i,1) =  + 0.8*y(i-1,1) + randn(1,1);
end;

% carry out test
orders = [1 2 3 4 5 6];
[qstat pval] = qstat2(y,orders);

% print out results
disp('Box-Ljung Q-test results for autocorrelated series');
info.cnames = strvcat('Q-stats','p-values');
rnames = 'Order';
for j=1:length(orders);
rnames = strvcat(rnames,num2str(j));
end;
info.rnames = rnames;
mprint([qstat' pval'],info);

% now create slightly-serially correlated time-series
y = zeros(nobs,1);
y(1,1) = randn(1,1);
tt = 1:nobs;
tt = tt';
for i=2:nobs
y(i,1) =  + 0.1*y(i-1,1) + 10*randn(1,1);
end;

% carry out test
orders = [1 2 3 4 5 6];
[qstat pval] = qstat2(y,orders);

% print out results
disp('Box-Ljung Q-test results for slightly autocorrelated series');
info.cnames = strvcat('Q-stats','p-values');
rnames = 'Order';
for j=1:length(orders);
rnames = strvcat(rnames,num2str(j));
end;
info.rnames = rnames;
mprint([qstat' pval'],info);
