function plot_quad(A,B,c,M,FARBE)
% Plots solutions of curves given implicetely by a quadrik
% x^TAx + b^Tx + c, A symmetric (2,2)-matrix

plot(-M,-M,'k.'), plot(M,M,'k.'), hold on 
axis([-M M -M M]) %Boundary of Figure
axis equal, axis manual
grid on
a11 = A(1,1); a12 = A(1,2); a22 = A(2,2);
b1 = B(1); b2 = B(2);
%a22*x2^2 + (2*a12*x1 + b2)*x2 + (a11*x1^2 + b1*x1 + c)
%a11*x1^2 + (2*a12*x2 + b1)*x1 + (a22*x2^2 + b2*x2 + c) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% X-axis ---------- %
XA = [-M,M]; YA = [0,0];
arrow(XA,YA,M/20,M/40,'k',1)
% Y-axis ---------- %
XA = [0,0]; YA = [-M,M];
arrow(XA,YA,M/20,M/40,'k',1)

if norm(A) == 0
   dis(' not a quadrik! '), return
end
NN = 1000; 
CURVE1 = zeros(2,NN); CURVE2 = zeros(2,NN);
X1 = linspace(-M,M,NN);
if a11 == 0 & a22 == 0
   X2 = - (b1*X1 + c)./(2*a12*X1 + b2);
   plot(X1,X2,FARBE,'linewidth',2), hold on
end
CURVE1 = []; CURVE2 = [];
if a11 ~= 0 & a22 ~= 0
   X2 = linspace(-M,M,NN);
   for K = 1:length(X2)
      A = a11; B = 2*a12*X2(K) + b1; C = a22*X2(K)^2 + b2*X2(K) + c;
      DISKR = B^2 - 4*A*C;
      if DISKR == 0
         CURVE1 = [CURVE1,[-B/(2*A); X2(K)]];
         CURVE2 = [CURVE2,[-B/(2*A); X2(K)]];
      end
      if DISKR > 0 
         CURVE1 = [CURVE1,[(-B + sqrt(DISKR))/(2*A); X2(K)]];
         CURVE2 = [CURVE2,[(-B - sqrt(DISKR))/(2*A); X2(K)]];
      end
   end
end
if a11 == 0 & a22 ~= 0
   X1 = linspace(-M,M,NN);
   for K = 1:length(X1)
      A = a22; B = 2*a12*X1(K) + b2; C = a11*X1(K)^2 + b1*X1(K) + c;
      DISKR = B^2 - 4*A*C;
      if DISKR == 0
         CURVE1 = [CURVE1,[-B/(2*A); X2(K)]];
         CURVE2 = [CURVE2,[-B/(2*A); X2(K)]];
      end
      if DISKR > 0 
         CURVE1 = [CURVE1,[(-B + sqrt(DISKR))/(2*A); X2(K)]];
         CURVE2 = [CURVE2,[(-B - sqrt(DISKR))/(2*A); X2(K)]];
      end
   end   
end
if a11 ~= 0 & a22 == 0
   X1 = linspace(-M,M,NN);
   J = find((2*a12*X1 + b2) ~= 0);
   X1 = X1(J);
   X2 = - (a11*X1.^2 + b1*X1 + c)./(2*a12*X1 + b2);
   CURVE1 = [X1;X2];
end   
if ~isempty(CURVE1)
   plot(CURVE1(1,:),CURVE1(2,:),FARBE,'linewidth',2), hold on
end
if ~isempty(CURVE2)
   plot(CURVE2(1,:),CURVE2(2,:),FARBE,'linewidth',2), hold on
end
grid off

