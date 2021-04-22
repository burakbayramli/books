%Define the function
f=@(x) -0.1*x^4 - 0.15*x^3-0.5*x^2-0.25*x +1.2;
%Define Step size
h=0.25;
%Set point at which to evaluate the derivative
x = 0.5;
%% Using forward difference
%First order:
df=(f(x+h)-f(x)) / h;
fprintf('\n\nFirst order Forward difference: %g, with error:%g%% \n',df,abs(100*(df+0.9125)/0.9125))
%Second order:
df=(-f(x+2*h)+4*f(x+h)-3*f(x)) / (2*h);
fprintf('Second order Forward difference: %g, with error:%g%% \n',df,abs(100*(df+0.9125)/0.9125))
%% Backwards difference
%First order:
df=(-f(x-h)+f(x)) / (h);
fprintf('First order Backwards difference: %g, with error:%g%% \n',df,abs(100*(df+0.9125)/0.9125))
%Second order:
df=(f(x-2*h)-4*f(x-h)+3*f(x)) / (2*h);
fprintf('Second order Backwards difference: %g, with error:%g%% \n',df,abs(100*(df+0.9125)/0.9125))
%% Central difference
%Second order:
df=(f(x+h)-f(x-h)) / (2*h);
fprintf('Second order Central difference: %g, with error:%g%% \n',df,abs(100*(df+0.9125)/0.9125))
%Fourth order:
df=(-f(x+2*h)+8*f(x+h)-8*f(x-h)+f(x-2*h)) / (12*h);
fprintf('Fourth order Central difference: %g, with error:%g%% \n',df,abs(100*(df+0.9125)/0.9125))