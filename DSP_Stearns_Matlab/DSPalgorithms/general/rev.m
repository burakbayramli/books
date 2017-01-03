function y=rev(x);
% y=rev(x)
%
% If x is a vector, y = x with elements in revese order.
%
% If x is an array, y = x with elements in each column
% reversed, and then the columns themselves reversed.

[r,c]=size(x);
v=x(r:-1:1,:);
y=v(:,c:-1:1);
