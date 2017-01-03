% PURPOSE: demo of probit()
%       probit maximum likelihood estimation
%       (data from Spector and Mazzeo, 1980) 
%---------------------------------------------------
% USAGE: probit_d
%---------------------------------------------------

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

vnames=['grade  ',
        'iota   ',
        'psi    ',
        'tuce   ',
        'gpa    '];


reso = ols(y,x);
prt(reso,vnames);
% results reported in Green (1997, chapter 19)
% b = [ -1.498, 0.379, 0.010, 0.464 ]

resl = logit(y,x);

prt(resl,vnames);
% results reported in Green (1997, chapter 19)
% b = [-13.021, 2.379, 0.095, 2.826 ]

resp = probit(y,x);

prt(resp,vnames);
% results reported in Green (1997, chapter 19)
% b = [-7.452, 1.426, 0.052, 1.626 ]



