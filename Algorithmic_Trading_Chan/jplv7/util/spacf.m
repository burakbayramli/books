function x = spacf(y,m)
% PURPOSE: find sample partial autocorrelation coefficients 
%---------------------------------------------------
% USAGE: p = spacf(y,m)
% where: y = a time-series (need not have mean zero)
%        m = # of sample partial autocorrelations to compute
%---------------------------------------------------
% RETURNS:
%        x - an m x 1 vector of sample pacf's
%        plots the spacf values with 2-sigma intervals
% --------------------------------------------------
% SEE ALSO: sacf(y,m)
%---------------------------------------------------
 
if nargin ~= 2
error('Wrong # of arguments to spacf');
end;

n = length(y);
x = zeros(m,1);
npm = n+m;

% put y in deviations from mean form
ym = mean(y);
e = zeros(npm,1);
e(1:n,1) = y - ones(n,1)*ym;

f = crlag(e,npm);

for i=1:m
part = (f'*e)/(f'*f);
apart = -part;
tmp = e;
e = tmp + apart*f;
f = f + apart*tmp;
f = crlag(f,npm);
x(i,1) = part;
end;

ul = 2*(1/sqrt(n)*ones(m,1));
ll = -2*(1/sqrt(n)*ones(m,1));

bar(x);
title('Sample partial autocorrelation coefficients');
xlabel('k-values');
ylabel('spacf values');
hold on;
tt=1:m;
plot(tt,ul,'*r',tt,ll,'*r');
hold off;







