% PURPOSE: An example of using sarp_g() Gibbs sampling
%          spatial autoregressive probit model
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: sarp_gd (see also sarp_gd2 for a large data set)
%---------------------------------------------------

clear all;

% generated data
n = 400;

defaultStream = RandStream.getDefaultStream;

latt = rand(n,1);
long = rand(n,1);
W = make_neighborsw(latt,long,5); % 5 nearest neighbors weight matrix

IN = speye(n); 
rho = 0.8;  % true value of rho
sige = 1;
k = 3;

x = [ones(n,1) 5*randn(n,k)];

xsort = [];
for i=1:k+1;
tmp = sort(x(:,i));
xsort = [xsort tmp];
end;

x = xsort;


beta(1,1) = 0.0;
beta(2,1) = 1.0;
beta(3,1) = -1.0;
beta(4,1) = 1.0;
theta = 2*beta(2:end,1);

y = (IN-rho*W)\(x*beta) + (IN-rho*W)\randn(n,1);

disp('maximum likelihood estimates based on continuous y');
result = sar(y,x,W);
prt(result);

z = (y > 0);
z = ones(n,1).*z; % eliminate a logical vector

disp('# of zeros and ones');
[n-sum(z) sum(z)]

% Gibbs sampling function homoscedastic prior
% to maximum likelihood estimates
ndraw = 600;
nomit = 100;

prior2.nsteps = 1;
result2 = sarp_g(z,x,W,ndraw,nomit,prior2);
prt(result2);


total_obs1 = mean((result2.total_obs(:,1)));

total1 = mean(total_obs1)

tt=1:n;
plot(tt,total_obs1,tt,ones(1,n)*total1,'*');
legend('observation level total effect','mean total effect');
title('variable 1');
pause;

total_obs2 = mean((result2.total_obs(:,2)));

total2 = mean(total_obs2)

tt=1:n;
plot(tt,total_obs2,tt,ones(1,n)*total2,'*');
legend('observation level total effect','mean total effect');
title('variable 2');
pause;

total_obs3 = mean((result2.total_obs(:,3)));

total3 = mean(total_obs3)

tt=1:n;
plot(tt,total_obs3,tt,ones(1,n)*total3,'*');
legend('observation level total effect','mean total effect');
title('variable 3');


