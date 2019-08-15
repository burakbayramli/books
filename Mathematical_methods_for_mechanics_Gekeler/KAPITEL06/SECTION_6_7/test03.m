function test03
% top with Euler angles
% check of cases 2, 4, 5, 6
% x1 = cos(theta1); x2 = cos(theta2);

clc
flag = 4;
if flag == 2
   clf
  % theta1 = pi/2, theta2 = 3*pi/4;
  x1 = 0; x2 = -sqrt(2)/2;
  a = -2.5; b = 1; d3 = a; D3 = b; alfa = a*a;
   x = x2;
   beta = (alfa - (a-b*x)^2/(1 - x*x))/x
   TT = linspace(-5,-sqrt(2)/2,40); x = - sqrt(2)/2;
   YY = TT.*TT*x^2 - 2*x*TT + x^2;
   plot(TT,YY), hold on
   XX = [-5,-sqrt(2)/2]; YY = [0,0];
   plot(XX,YY)
end
if flag == 4 % Fall 4 %GEHT HIER NICHT
   theta1 = pi/6; theta2 = pi/3; b = 2; % FAILS
   x1 = cos(theta1); x2 = cos(theta2), a = x2*b;
  A = [1, -x1; 1, -x2];
  B = [(a - b*x1)^2/(1-x1^2); (a - b*x2)^2/(1-x2^2)]
  X = A\B
end
if flag == 5 % Fall 5
  theta1 = pi/4; theta2 = pi/2;
  x1 = cos(theta1); x2 = cos(theta2); %x2 = 0
  a = 1; b = 2; alfa = a*a;
   x = x1;
   beta = (alfa - (a-b*x)^2/(1 - x*x))/x

  %A = [1-x1^2, -x1*(1-x1^2); 1-x2^2, -x2*(1-x2^2)];
  %B = [(d3 - D3*x1)^2; (d3 - D3*x2)^2];
  %X = A\B
end
if flag == 6 % Fall 6
  % alfa, beta aus f und Ableitung von f
  theta1 = pi/4; theta2 = pi/4;
  x = cos(theta1); a = 1.2; b = 1; d3 = a; D3 = b;
  A = [1-x^2, -x *(1-x^2);
        -2*x, -1+3*x^2];
  B = [(d3 - D3*x )^2; -2*D3*(d3 - D3*x)];
  X = A\B
end
