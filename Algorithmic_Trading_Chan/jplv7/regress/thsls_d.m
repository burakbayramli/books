% PURPOSE: An example using thsls(),
%                           prt_eqs(), plt_eqs()
% Three-stage least-squares estimation                              
%---------------------------------------------------
% USAGE: thsls_d
%---------------------------------------------------

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

tic;
result = thsls(neqs,y,Y,X);
toc;

vname = [vname1
         vname2
         vname3];

prt_eqs(result,vname);


