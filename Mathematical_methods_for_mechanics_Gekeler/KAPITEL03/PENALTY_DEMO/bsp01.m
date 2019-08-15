function Y = bsp01(X,flag,Parmeter)
% Example Spellucci, S.456
% flag = 1 : objective function
% flag = 2 : constraint
% flag = 3 : Zangwill's penalty function
% flag = 4 : classic penalty function
BETA = Parmeter(1); 
% -- Objective Function ------------
F  = (X.*(X - 1).*(X - 3).*(X - 5))/8;
% -- Inequality Constraint ---------
G = X.*(4 - X);
NULL = zeros(1,length(X));
UNIT = ones(1,length(X));
switch flag
case 1, Y = F;
case 2, Y = G;
case 3, Y = F + BETA*max(NULL,-G);
case 4, Y = F + BETA*(max(NULL,-G)).^2;
end
