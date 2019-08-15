function fig0408a(startwert)

%disp(' Zuerst DEMO1-4 aufrufen ! ')
clf
switch startwert
   case 1, load daten04a X Parmeter
   case 2, load daten04b X Parmeter
   case 3, load daten04c X Parmeter
   case 4, load daten04d X Parmeter
end
% -- X-Achse --------------
c = 0.3; d = 0.15;
X1 = [-6.5,6.5]; Y1 = [0, 0];
arrow(X1,Y1,c,d,'k',2)
% -- Y-Achse -------------
X1 = [0, 0]; Y1 = [-6, 6];
arrow(X1,Y1,c,d,'k',2)

TT = linspace(0,pi,40);
X2 = cos(TT);
Y2 = - sin(TT);
plot(1+X2,Y2,'k'), hold on
plot(3+X2,Y2,'k'), hold on
plot(5+ X2,Y2,'k'), hold on
plot(-1 + X2,-Y2,'k'), hold on
plot(-3 + X2,-Y2,'k'), hold on
plot(-5 + X2,-Y2,'k'), hold on

n  = Parmeter(1);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
X4 = X(3*n+4);
plot(X1,X2,'k'), hold on
plot(X1,X2,'.','Markersize',6);
text(3.7,4.5,'u = - 1','fontsize',20)
text(3.7,-4.5,'u = + 1','fontsize',20)
ZEIT = X4
axis equal, grid on
flag = 0;
if flag == 1
   pause
   disp(' Residuum von h ')
   HH = feval('bsp04',X,3,Parmeter);
   FF = Parmeter(7);
   HH = HH/FF;
   clf
   plot([1:length(HH)]',HH),hold on
   plot([1:length(HH)]',HH,'k.'),hold on
   title(' Residuum von h ')
   grid on
   pause
   disp(' Control ')
   clf
   X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
   X4 = X(3*n+4);
   plot(X3,'k'), hold on
   plot(X3,'.','Markersize',6);
   title('Control')
   grid on
end
