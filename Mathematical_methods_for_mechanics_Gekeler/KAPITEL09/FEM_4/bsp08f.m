function [X,Y,P] = bsp08f(segnr)
% Benard cell, Randsegmente
% A = [X-Werte; Y-Werte];
A1 = [0, 1; 0, 0];
A2 = [1, 1; 0, 1];
A3 = [1, 0; 1, 1];
A4 = [0, 0; 1, 0];
switch segnr
case 1, A = A1;
case 2, A = A2;
case 3, A = A3;
case 4, A = A4;
case  5, A = A1; A(1,:) = A1(1,:) + 1;
case  6, A = A2; A(1,:) = A2(1,:) + 1;
case  7, A = A3; A(1,:) = A3(1,:) + 1;

case  8, A = A1; A(1,:) = A1(1,:) + 2;
case  9, A = A2; A(1,:) = A2(1,:) + 2;
case 10, A = A3; A(1,:) = A3(1,:) + 2;

case 11, A = A1; A(1,:) = A1(1,:) + 3;
case 12, A = A2; A(1,:) = A2(1,:) + 3;
case 13, A = A3; A(1,:) = A3(1,:) + 3;

case 14, A = A1; A(1,:) = A1(1,:) + 4;
case 15, A = A2; A(1,:) = A2(1,:) + 4;
case 16, A = A3; A(1,:) = A3(1,:) + 4;

case 17, A = A1; A(1,:) = A1(1,:) + 5;
case 18, A = A2; A(1,:) = A2(1,:) + 5;
case 19, A = A3; A(1,:) = A3(1,:) + 5;

case 20, A = A1; A(1,:) = A1(1,:) + 6;
case 21, A = A2; A(1,:) = A2(1,:) + 6;
case 22, A = A3; A(1,:) = A3(1,:) + 6;

case 23, A = A1; A(1,:) = A1(1,:) + 7;
case 24, A = A2; A(1,:) = A2(1,:) + 7;
case 25, A = A3; A(1,:) = A3(1,:) + 7;

case 26, A = A1; A(1,:) = A1(1,:) + 8;
case 27, A = A2; A(1,:) = A2(1,:) + 8;
case 28, A = A3; A(1,:) = A3(1,:) + 8;

case 29, A = A1; A(1,:) = A1(1,:) + 9;
case 30, A = A2; A(1,:) = A2(1,:) + 9;
case 31, A = A3; A(1,:) = A3(1,:) + 9;

case 32, A = A1; A(1,:) = A1(1,:) + 10;
case 33, A = A2; A(1,:) = A2(1,:) + 10;
case 34, A = A3; A(1,:) = A3(1,:) + 10;

case 35, A = A1; A(1,:) = A1(1,:) + 11;
case 36, A = A2; A(1,:) = A2(1,:) + 11;
case 37, A = A3; A(1,:) = A3(1,:) + 11;
end
N = size(A,2); L = 0; AUX = zeros(1,N); P = AUX;
for I = 1:N-1
    % ungefaehre Laenge der Segmente
    AUX(I) = sqrt((A(1,I+1) - A(1,I))^2  + (A(2,I+1) - A(2,I))^2);
end
L = sum(AUX); AUX = AUX/L;
for I = 2:N
    P(I) = P(I-1) + AUX(I-1);
end
X = A(1,:); Y = A(2,:);
