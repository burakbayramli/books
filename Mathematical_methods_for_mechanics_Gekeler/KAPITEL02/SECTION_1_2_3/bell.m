function Y = bell(X,Y,P,DEGREE)
% Direct Integration of a two-dimensional polynomial over a triangle
% X,Y   : Coordinates of triangle
% P     : Coefficients of polynomial, sequence as in POLYNOM.M
% DEGREE: Arbitrary degree of polynomial

KOEFF = prs(X,Y,0,0); 
for I = 1:DEGREE
   for K = 1:I+1
      KOEFF = [KOEFF;prs(X,Y,I-K+1,K-1)];
   end
end
P = P(:); Y = P.*KOEFF; Y = sum(Y); 
