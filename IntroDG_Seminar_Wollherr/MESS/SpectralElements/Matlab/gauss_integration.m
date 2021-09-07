
clear 
close all

% Exercise for Gauss integration


% Initialize space in [-1 1]
n=1000;
x=linspace(-1,1,n);
f=x.*x;   % f=x^2

% The antiderivative of f = x^2 is [1/3 x^3] 
intf = 1/3 * (1)^3 - 1/3 * (-1)^3;     % Value of integral

% Calculate integral numerically
% Choose order
N = input(' Give order of integration ');

% Get integration points and weights
[xi,w] = gll(N);
    
% Initialize function at points xi
fi = xi.*xi;

% Evaluate integral
intfn = 0;
for i=1:length(w);
    intfn = intfn + w(i)*fi(i);
end

% Plot results

plot(x,f,'k-',xi,fi,'d')
xlabel('x')
ylabel('f(x)')
title(sprintf(' Analytical: %g ; Numerical: %g ; Error: %g %%',intf,intfn,abs(intfn-intf)/intf*100))
   