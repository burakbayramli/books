function xinvpxfit
% xinvpxfit  Least squares fit of synthetic data to y = c1/x + c2*x
%            Computations from Example 9.5
%
% Synopsis:  xinvpxfit
%
% Input:  none
%
% Output:  Curve fit coefficients obtained by normal equations and QR
%          Plot of fit and original data

% --- store data in column vectors
D = load('xinvpx.dat');       %   Contents of file stored in D matrix
x = D(:,1);  y = D(:,2);      %   Copy 1st and 2nd columns into x and y

Afun = inline('[1./x  x]');   %  Inline definition of function to eval A

% --- perform the fit two ways
[cn,R2n,rn] = fitnorm(x,y,'xinvpxBasis');  %  via normal equations
[c,R2,r]    = fitqr(x,y,'xinvpxBasis');    %  via QR factorization

% --- print comparision of solutions
fprintf('             QR factor      Normal Eqns      difference\n');
for k=1:2
  fprintf(' c(%d)     %14.10f  %14.10f  %12.2e\n',k,c(k),cn(k),c(k)-cn(k));
end
fprintf('||r||_2   %14.10f  %14.10f  %12.2e\n',norm(r),norm(rn),norm(r-rn));

% --- evaluate and plot the fit function
xf = linspace(min(x),max(x))'; %  100 points in range of original x data
Af = Afun(xf);                 %  Eval basis fcns at xf to form columns of A
yf = Af*c;                     %  Evaluate the fit function at xf
plot(x,y,'o',xf,yf,'-');
legend('data','fit',2)
xlabel('x');  ylabel('y');
