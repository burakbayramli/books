% hermite_ex.m
% This MATLAB routine generates the Hermite
% interpolating polynomial to a function f(x)
% in the domain [a, b], fitting the values of
% the function and its first derivative at each
% end point.
% Written by Kenneth J. Beers
% MIT Department of Chemical Engineering
% March 27, 2005

function iflag = hermite_ex(fun_name,a,b);
iflag = 0;


% evaluate the function at each end point
% and use finite differences to estimate
% first derivatives
f_a = feval(fun_name,a);
dx = sqrt(eps);
f1_a = (feval(fun_name,a+dx) - f_a)/dx;
f_b = feval(fun_name,b);
f1_b = (f_b - feval(fun_name,b-dx))/dx;

% make a plot on (a,b) of the function and
% its Hermite interpolating polynomial
N = 500;
x = linspace(a,b,500);
f = zeros(size(x));  p = zeros(size(x));
for k=1:N
    f(k) = feval(fun_name,x(k));
    [L00, L01, L10, L11] = hermite_poly(x(k),a,b);
    p(k) = f_a*L00 + f1_a*L01 + f_b*L10 + f1_b*L11;
end

% plot the function and the polynomial
figure;  plot(x,f);  hold on;
plot(x,p,'-.');
xlabel('x');  ylabel('f(x), p(x)');
title('f(x) and Hermite interpolating polynomial p(x)');
legend('f(x)', 'p(x)', 'Location','Best');



iflag = 1;
return;


% ------------------------------
% This routine returns the values at x of the Hermite
% interpolating polynomials that fits the values
% of f(x) and f'(x) at x0 = a and x1 = b.
function [L00, L01, L10, L11] = hermite_poly(x,a,b);

var0 = ((x-b)/(a-b))^2;  var1 = ((x-a)/(b-a))^2;

L01 = (x-a)*var0;
L11 = (x-b)*var1;

L00 = var0 - 2/(a-b)*(x-a)*var0;
L10 = var1 - 2/(b-a)*(x-b)*var1;

return;
