function Y = test04(X,flag,Parmeter)
% Test problem, cf. Spellucci, S.457
% flag = 1 : objective function
% flag = 2 : constraint
% flag = 3 : Zangwill's penalty function
% flag = 4 : classic penalty function

BETA1 = Parmeter(1); %20;
BETA2 = Parmeter(2); %2;
% -- Objective Function ----------------
F = (X(1,:) - X(2,:)).^2 + (X(2,:) - 1).^2;
% -- Constraints -----------------------
G = [1 + X(2,:) - X(1,:).^2; 1 - X(2,:) - X(1,:).^2];
switch flag
case 1, Y = F;
case 2, Y = G; 
case 3, Y = F + BETA1*max(0,-G(1,:)) + BETA2*max(0,-G(2,:));
case 4, Y = F + BETA2*(max(0,-G(1,:))).^2 + BETA2*(max(0,-G(2,:))).^2;
case 5 % nur fuer Plot
   X = linspace(-1,1,20);
   Y1 = X.*X - 1;
   Y2 = 1 - X.*X;
   Y = [X;Y1;Y2];
end
