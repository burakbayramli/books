function Y = bsp04(X,flag,Parmeter);
% Example of  Allgower/Georg
% flag = 1: Function
% flag = 2: Gradient of function

n1 = length(X); n = n1 - 1;
S = sum(X); S = S - X(n1);
switch flag
case 1
   I = 1:n; I = I'; U = X(I);
   Y = U - X(n1)*exp(cos(S*I));
case 2
   Z = 1:n;
   U = exp(cos(S*Z)); V = sin(S*Z);
   Y = eye(n);
   for k = 1:n
      Y(k,:) = Y(k,:) + X(n1)*U.*V.*Z;
   end;
   Y = [Y; -U]';
end
