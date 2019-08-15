function Y = bsp02(X,flag,Parmeter)
% Test problem, cf. Spellucci, S.394
% flag = 1 : objective function
% flag = 2 : constraint
% flag = 3 : Zangwill's penalty function
% flag = 4 : classic penalty function
GAMMA = Parmeter;
% -- Objective Function ----------------------
F = (X(1,:) - 5).^2 + 4*(X(1,:) - 5).*X(2,:) + 5*X(2,:).^2;
% -- Equality Constraint ---------------------
H = 2 - X(1,:) - X(2,:); 
switch flag
case 1, Y = F;
case 2, Y = H;
case 3, Y = F + GAMMA*abs(H);
case 4, Y = F + GAMMA*H^2;
end
