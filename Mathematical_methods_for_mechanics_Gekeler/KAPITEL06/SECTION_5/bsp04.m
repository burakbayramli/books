function Y = bsp04(T,X,Parmeter)
% restricted three-body problem of Arenstorf
MU = Parmeter; MU1 = 1 - MU; Y = zeros(4,1);
DEN1 = ((X(1) + MU)^2 + X(2)^2)^(3/2);
DEN2 = ((X(1) - MU1)^2 + X(2)^2)^(3/2);
Y(1:2) = X(3:4);
Y(3)   = X(1) + 2*X(4) - MU1*(X(1)+ MU)/DEN1 - MU*(X(1) - MU1)/DEN2;
Y(4)   = X(2) - 2*X(3) - MU1*X(2)/DEN1       - MU*X(2)/DEN2;
  