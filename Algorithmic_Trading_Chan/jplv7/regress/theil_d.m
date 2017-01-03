% PURPOSE: An example using theil(),
%                           prt_reg(),
%                           plt_reg(),
% Theil-Golberger mixed estimation
%---------------------------------------------------
% USAGE: theil_d
%---------------------------------------------------

clear all;
% generate a data set
nobs = 100;
nvar = 5;
beta = ones(nvar,1);

xmat = randn(nobs,nvar-1);
x = [ones(nobs,1) xmat];
evec = randn(nobs,1);

y = x*beta + evec;


Vnames = ['y    ',
          'const',
          'x1   ',
          'x2   ',
          'x3   ',
          'x4   '];
          
res = ols(y,x);

nvar = res.nvar;

prt(res,Vnames);

% set up prior
rvec = [ 1.0    % prior means for the coefficients
         1.0    
         1.0    
         1.0    
         1.0];
        
rmat = eye(nvar);
bv = 10000.0;

% umat1 = loose prior
% umat2 = medium prior
% umat3 = tight prior

umat1 = eye(nvar)*bv; % initialize prior variance as diffuse

for i=1:nvar;
umat1(i,i) = 1.0;     % overwrite diffuse priors with informative prior
end;

umat2 = eye(nvar)*bv; % initialize prior variance as diffuse

for i=1:nvar;
umat2(i,i) = 0.1;     % overwrite diffuse priors with informative prior
end;

umat3 = eye(nvar)*bv; % initialize prior variance as diffuse

for i=1:nvar
umat3(i,i) = 0.01;    % overwrite diffuse priors with informative prior
end;

lres = theil(y,x,rvec,rmat,umat1);

prt(lres,Vnames);

mres = theil(y,x,rvec,rmat,umat2);

prt(mres,Vnames);

tres = theil(y,x,rvec,rmat,umat3);

prt(tres,Vnames);

