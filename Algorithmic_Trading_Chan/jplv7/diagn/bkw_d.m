% PURPOSE: demo of bkw() 
%          Belsley, Kuh, Welsch (1980)
%          Collinearity diagnostic routine
% 
%---------------------------------------------------
% USAGE: bkw_d
%---------------------------------------------------

% generate collinear data set
n = 200;
k = 13;
x = randn(n,k);
x(:,1) = ones(n,1);
x(:,3) = x(:,2) + randn(n,1)*0.05;

vnames = ['vname1 ',
          'vname2 ',
          'vname3 ',
          'vname4 ',
          'vname5 ',
          'vname6 ',
          'vname7 ',
          'vname8 ',
          'vname9 ',
          'vname10',
          'vname11',
          'vname12',
          'vname13'];

% demonstrate BKW collinearity diagnostics
disp('results with no variable names');
          bkw(x(:,1:5));
disp('results with variable names');          
          bkw(x(:,1:5),vnames(1:5,:));
          

x = randn(50,5);
x(:,2) = x(:,1) + randn(50,1)*0.001;

vnames =strvcat('y-variable','Illinois','Ohio','Indiana',...
         'W. Virginia','Pennsylvania');
disp('results with default format');     
          bkw(x,vnames);

fmt = '%12.6f';
disp('results with user-supplied format');
          bkw(x,vnames,fmt);


% load bauer matrix from page 110 Belsley, Kuh, Welsch
load bauer.dat;
% scale so columns have unit length
bauers = studentize(bauer);
disp('results for modified Bauer matrix from BKW');
bkw(bauers);

