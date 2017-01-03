% test tobit using Koop example

clear all;

n = 300;

%generate explanatory variable

a = 0;
b = 1;

x= a+(b-a)*rand(n,1);

x = [ones(n,1) x];

beta=[-1 
       2];

sigma1= 2;

latt = randn(n,1);
long = randn(n,1);

W = make_neighborsw(latt,long,6);
rho = 0.7;

S = (speye(n) - rho*W)\eye(n);

e = randn(n,1)*sqrt(sigma1);

y = S*(x*beta) + S*e;
ysave = y;
[ysort,yind] = sort(ysave);

ind = find(y <=0);
y(ind,1) = 0;
nobsc = length(ind);

yin = y;

disp('percent zero');
length(ind)/n

nmiss = length(ind)/n;
adjust = 1/(1-nmiss);


k = size(x,2);

vnames = strvcat('y koop','constant=-1','beta=2');


ndraw = 1500;
nomit = 500;

% run standard model
info.lflag = 0;
info.rmin = -0.99;
info.rmax = 0.99;
result = sar(ysave,x,W,info);
fprintf(1,'Results based on non-truncated y-vector \n');
prt(result,vnames);


prior.nsteps = 5;

result = sart_g(y,x,W,ndraw,nomit,prior);
prt(result,vnames);

