% PURPOSE: An example using fmin function
%                           
%  to solve a spatial autoregressive model maximum
%  likelihood problem                           
%---------------------------------------------------
% USAGE: optim2_d
%---------------------------------------------------

clear all;

load anselin.dat;
% y =  dependent variable
% x = a matrix of indepdendent variables

y = anselin(:,1);
ydev = y - mean(y);
n = length(y);
xc = anselin(:,4);
yc = anselin(:,5);

% construct Anselin (1988) 1st order contiguity matrix
[j1 W j2] = xy2cont(xc,yc);

rmin = -1;
rmax = 1;
out = lndetfull(W,rmin,rmax);
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval = [tt' outi];


% step 1) maximize concentrated likelihood function;
 options = optimset('fminbnd');

 [p fval exitflag] = fminbnd('f_far',-1,1,options,y,W,detval);

liktmp = fval

 if exitflag ~= 1
 fprintf(1,'far: convergence not obtained in %4d iterations \n',optimget('MaxIter'));
 end;


% step 2) find sige
Wy = sparse(W)*y;
epe = (y - p*Wy)'*(y-p*Wy); sige = epe/n; 
rho = p;
yhat = p*Wy;
resid = y - yhat; 
lik = -(liktmp + (n/2)*log(sige));



% asymptotic t-stats 
parm = [p
        sige];

hessn = hessian('f2_far',parm,y,W,detval);

xpxi = inv(hessn);
tstat = p/sqrt(xpxi(1,1));

in.cnames = strvcat('rho','t-statistic');
mprint([rho tstat],in);

fprintf(1,'sigma value');
mprint(sige);


fprintf(1,'-log likelihood function value');
mprint(lik);