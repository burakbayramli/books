function Y = bsp01(X,flag,Parmeter);
% Niveau lines f(X(1),X(2)) = f(X0(1),X0(2))
% flag = 1: Function
% flag = 2: Gradient of function
% flag = 3: Function value for niveau line
% -------------------------------------------
if ~isempty(Parmeter)
   F0 = Parmeter;
end
switch flag
case 1
%   Y = X(1).^4 + X(2).^4 + 2*X(1).^2.*X(2).^2 - 2*X(1).^2 + 2*X(2).^2;
   Y = X(1).^4 + X(2).^4 + 2*X(1).^2.*X(2).^2 - 2*X(1).^2 + 2*X(2).^2;
   Y = -Y - F0;
case 2
   Y = [4*X(1)^3+4*X(1)*X(2)^2-4*X(1), ...
        4*X(2)^3+4*X(1)^2*X(2)+4*X(2)];
   Y = - Y;
case 3
   Y = X(1).^4 + X(2).^4 + 2*X(1).^2.*X(2).^2 - 2*X(1).^2 + 2*X(2).^2;
   Y = - Y;
end
