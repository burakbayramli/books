% PURPOSE: demo using most all regression functions
%                       
%       ols,hwhite,nwest,ridge,theil,tsls,logit,probit,tobit,robust
%---------------------------------------------------
% USAGE: demo_all
%---------------------------------------------------
clear all;

rand('seed',10);
n = 100; k=3;
xtmp = randn(n,k-1);
tt = 1:n;
ttp = tt';

e = randn(n,1).*ttp; % heteroscedastic error term
%e = randn(n,1);     % homoscedastic error term
b = ones(k,1);
iota = ones(n,1);
x = [iota xtmp];
% generate y-data
y = x*b + e;

vnames=strvcat('yvar','iota','x1','x2');

%  * * * * * * *  demo ols regression
reso = ols(y,x);
prt(reso,vnames);

%  * * * * * * *  demo hwhite regression
res = hwhite(y,x);
prt(res,vnames);

%  * * * * * * *  demo nwest regression
nlag=2;
res = nwest(y,x,nlag);
prt(res,vnames);

%  * * * * * * *  demo ridge regresson
rres = ridge(y,x);
prt(rres,vnames);

% * * * * * * *  demo logit regression
n = 24;
y = zeros(n,1);
y(1:14,1) = ones(14,1);
%       (data from Spector and Mazzeo, 1980) 
xdata = [21 24 25 26 28 31 33 34 35 37 43 49 ...
         51 55 25 29 43 44 46 46 51 55 56 58];
         
iota = ones(n,1);
x = [iota xdata'];      

vnames=strvcat('days','iota','response');

res = logit(y,x);
prt(res,vnames);

% * * * * * * *  demo probit regression
n = 32; k=4;
y = zeros(n,1); % grade variable
y(5,1)  = 1;
y(10,1) = 1;
y(14,1) = 1;
y(20,1) = 1;
y(22,1) = 1;
y(25,1) = 1;
y(25:27,1) = ones(3,1);
y(29,1) = 1;
y(30,1) = 1;
y(32,1) = 1;

x = zeros(n,k);

x(1:n,1) = ones(n,1);      % intercept
x(19:32,2) = ones(n-18,1); % psi variable
tuce = [20 22 24 12 21 17 17 21 25 29 20 23 23 25 26 19 ...
        25 19 23 25 22 28 14 26 24 27 17 24 21 23 21 19];
        
x(1:n,3) = tuce';

gpa = [2.66 2.89 3.28 2.92 4.00 2.86 2.76 2.87 3.03 3.92 ...
       2.63 3.32 3.57 3.26 3.53 2.74 2.75 2.83 3.12 3.16 ...
       2.06 3.62 2.89 3.51 3.54 2.83 3.39 2.67 3.65 4.00 ...
       3.10 2.39];

x(1:n,4) = gpa';

vnames=strvcat('grade','iota','psi','tuce','gpa');

resp = probit(y,x);

prt(resp,vnames);
% results reported in Green (1997, chapter 19)
% b = [-7.452, 1.426, 0.052, 1.626 ]

%  * * * * * * *  demo theil-goldberger regression
% generate a data set
nobs = 100;
nvar = 5;
beta = ones(nvar,1);
beta(1,1) = -2.0;

xmat = randn(nobs,nvar-1);
x = [ones(nobs,1) xmat];
evec = randn(nobs,1);

y = x*beta + evec*10.0;

Vnames = strvcat('y','const','x1','x2','x3','x4');

% set up prior
rvec = [-1.0    % prior means for the coefficients
         1.0    
         2.0    
         2.0    
         1.0];
        
rmat = eye(nvar);
bv = 10000.0;

% umat1 = loose prior

umat1 = eye(nvar)*bv; % initialize prior variance as diffuse

for i=1:nvar;
umat1(i,i) = 1.0;     % overwrite diffuse priors with informative prior
end;

lres = theil(y,x,rvec,rmat,umat1);

prt(lres,Vnames);

%  * * * * * * *  demo two-stage least-squares regression

nobs = 200;

x1 = randn(nobs,1);
x2 = randn(nobs,1);
b1 = 1.0;
b2 = 1.0;
iota = ones(nobs,1);

y1 = zeros(nobs,1);
y2 = zeros(nobs,1);
evec = randn(nobs,1);

% create simultaneously determined variables y1,y2
for i=1:nobs;
y1(i,1) = iota(i,1)*1.0 + x1(i,1)*b1 + evec(i,1);
y2(i,1) = iota(i,1)*1.0 + y1(i,1)*1.0 + x2(i,1)*b2 + evec(i,1);
end;

vname2 = ['y2-eqn  ',
          'y1 var  ',
          'constant',
          'x2 var  '];
          
% use all exogenous in the system as instruments
xall = [iota x1 x2];            

% do tsls regression
result2 = tsls(y2,y1,[iota x2],xall); 
prt(result2,vname2);


% * * * * * * * demo robust regression

% generate data with 2 outliers

nobs = 100;
nvar = 3;

vnames = strvcat('y-variable','constant','x1','x2');

x = randn(nobs,nvar);

x(:,1) = ones(nobs,1);
beta = ones(nvar,1);
evec = randn(nobs,1);

y = x*beta + evec;

% put in 2 outliers
y(75,1) = 10.0;
y(90,1) = -10.0;

% get weighting parameter from OLS
% (of course you're free to do differently)
reso = ols(y,x);
sige = reso.sige;

% set up storage for bhat results
bsave = zeros(nvar,5);
bsave(:,1) = ones(nvar,1);

% loop over all methods producing estimates
for i=1:4;

wfunc = i;
wparm = 2*sige; % set weight to 2 sigma

res = robust(y,x,wfunc,wparm);

bsave(:,i+1) = res.beta;

end;
% column and row-names for mprint function
in.cnames = strvcat('Truth','Huber t','Ramsay','Andrews','Tukey');
in.rnames = strvcat('Parameter','constant','b1','b2');
fprintf(1,'Comparison of alternative robust estimators \n');
mprint(bsave,in);

res = robust(y,x,4,2);

prt(res,vnames);

%  * * * * * * *  demo regresson with t-distributed errors
res = olst(y,x);
prt(res,vnames);

%  * * * * * * *  demo lad regression
res = lad(y,x);
prt(res,vnames);

% * * * * * * * demo tobit regression
n=100; k=5;
x = randn(n,k);
x(:,1) = ones(n,1);
beta = ones(k,1)*0.5;
y = x*beta + randn(n,1);

% now censor the data
for i=1:n
 if y(i,1) < 0
 y(i,1) = 0.0;
 end;
end;

resp = tobit(y,x);

vnames = ['y     ',
          'iota  ',
          'x1var ',
          'x2var ',
          'x3var ',
          'x4var '];          
          
prt(resp,vnames);          


% * * * * * * * demo thsls regression

clear all;

nobs = 100;
neqs = 3;

x1 = randn(nobs,1);
x2 = randn(nobs,1);
x3 = randn(nobs,1);
b1 = 1.0;
b2 = 1.0;
b3 = 1.0;
iota = ones(nobs,1);

y1 = zeros(nobs,1);
y2 = zeros(nobs,1);
y3 = zeros(nobs,1);
evec = randn(nobs,3);
evec(:,2) = evec(:,3) + randn(nobs,1); % create cross-eqs corr

% create simultaneously determined variables y1,y2
for i=1:nobs;
y1(i,1) = iota(i,1)*10.0 + x1(i,1)*b1 + evec(i,1);
y2(i,1) = iota(i,1)*10.0 + y1(i,1)*1.0 + x2(i,1)*b2 + evec(i,2);
y3(i,1) = iota(i,1)*10.0 + y2(i,1)*1.0 + x2(i,1)*b2 + x3(i,1)*b3 + evec(i,3);
end;


vname1 = ['y1-LHS  ',
          'constant',          
          'x1 var  '];
          
vname2 = ['y2-LHS  ',
          'y1 var  ',
          'constant',
          'x2 var  '];
          
vname3 = ['y3-LHS  ',
          'y2 var  ',
          'constant',
          'x2 var  ',
          'x3 var  '];
                     

% set up a structure for y containing y's for each eqn
y(1).eq = y1;
y(2).eq = y2;
y(3).eq = y3;

% set up a structure for Y (RHS endogenous) for each eqn
Y(1).eq = [];
Y(2).eq = [y1];
Y(3).eq = [y2];

% set up a structure fo X (exogenous) in each eqn
X(1).eq = [iota x1];
X(2).eq = [iota x2];
X(3).eq = [iota x2 x3];

% do thsls regression

result = thsls(neqs,y,Y,X);

vname = [vname1
         vname2
         vname3];

prt(result,vname);

% * * * * * * * demo olsc, olsar1 regression

% generate a model with 1st order serial correlation
n = 200;
k = 3;
tt = 1:n;
evec = randn(n,1);
xmat = randn(n,k);
xmat(:,1) = ones(n,1);
beta = ones(k,1);
beta(1,1) = 10.0; % constant term
y = zeros(n,1);
u = zeros(n,1);

for i=2:n;
 u(i,1) = 0.4*u(i-1,1) + evec(i,1);
 y(i,1) = xmat(i,:)*beta + u(i,1);
end;

% truncate 1st 100 observations for startup
yt = y(101:n,1);
xt = xmat(101:n,:);
n = n-100; % reset n to reflect truncation
 
Vnames = ['y    ',
          'cterm',
          'x2   ',
          'x3   '];
          

% do Cochrane-Orcutt ar1 regression
result = olsc(yt,xt);
prt(result,Vnames);

% do maximum likelihood ar1 regression
result2 = olsar1(yt,xt);
prt(result2,Vnames);



% * * * * * * * demo switch_em, hmarkov_em regressions

clear all;

% generate data from switching regression model
nobs = 100; n1 = 3; n2 = 3; n3 = 3;
b1 = ones(n1,1); b2 = ones(n2,1)*5; b3 = ones(n3,1);
sig1 = 1; sig2 = 1;
randn('seed',201010);
x1 = randn(nobs,n1); x2 = randn(nobs,n2); x3 = randn(nobs,n3);
ytruth = zeros(nobs,1);
for i=1:nobs;
 if x3(i,:)*b3 <= 0
  y(i,1) = x1(i,:)*b1 + randn(1,1);
  ytruth(i,1) = 0;
 else
  y(i,1) = x2(i,:)*b2 + randn(1,1);
  ytruth(i,1) = 1;
 end;
end;

result = switch_em(y,x1,x2,x3,b1,b2,b3);

vnames1 = strvcat('y1','x1_1','x1_2','x1_3');
vnames2 = strvcat('y2','x2_1','x2_2','x2_3');
vnames3 = strvcat('x3_1','x3_2','x3_3');
vnames = [vnames1
          vnames2
          vnames3];

prt(result,vnames);

