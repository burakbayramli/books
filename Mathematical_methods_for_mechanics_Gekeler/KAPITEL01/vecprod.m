function Z = vecprod(X,Y)
Z = [X(2)*Y(3) - X(3)*Y(2);
     X(3)*Y(1) - X(1)*Y(3);
     X(1)*Y(2) - X(2)*Y(1)];
 