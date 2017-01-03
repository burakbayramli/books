function [rho,bigY] = sacf(y,m,gflag)
% PURPOSE: find sample autocorrelation coefficients 
%---------------------------------------------------
% USAGE: p = sacf(y,m,gflag)
% where: y = a time-series (need not have mean zero)
%        m = # of sample autocorrelations to compute
%    gflag = 0, flag for graphing, 1 = no graph
%            (default = 0, a graph is produced)
%---------------------------------------------------
% RETURNS:
%        p = an m x 1 vector of sample acf's
%        also plots sample acf's with 2-sigma intervals
%        if gflag == 0
% --------------------------------------------------
% SEE ALSO: spacf(y,m)
%---------------------------------------------------

% Originally from the JPL toolbox
% Vectorized by Kevin Sheppard
% kksheppard@ucsd.edu

if nargin == 2
flag = 0;
elseif nargin == 3
flag = gflag;
else
error('Wrong # of arguments to spacf');
end;

n = length(y);
rho = zeros(m,1);
npm = n+m;
tmp = std(y);
vary = tmp*tmp;

% put y in deviations from mean form
ym = mean(y);
e = zeros(n,1);
e(1:n,1) = y - ones(n,1)*ym;


bigY=repmat([1:n]',2*(m+1),1);
bigY=bigY(1:(2*n-1)*(m+1));
bigY=reshape(bigY,2*n-1,m+1);
bigY=bigY(1:n,2:m+1);
bigY=y(bigY);

E=repmat(e,1,m);
rho=(sum(E.*bigY)/(n*vary))';

ul = 2*(1/sqrt(n)*ones(m,1));
ll = -2*(1/sqrt(n)*ones(m,1));

if flag == 0
bar(rho);
title('Sample autocorrelation coefficients');
xlabel('k-values');
ylabel('sacf values');
hold on;
tt=1:m;
plot(tt,ul,'*r',tt,ll,'*r');
hold off;
end;



