function fig0416a
% Diagrams of Figure 4.16

bild = 100; KK = [1,2,3,4,5];
while ~ismember(bild,KK)
   bild = input(' Which Figure ? (1/2/3/4/5) ');
end
clf
load daten10 X Parmeter
n  = Parmeter(1);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
U  = X(3*n+4:4*(n+1));   % Control
switch bild
case 1, disp(' Velocity ')
   plot(X1,'k','linewidth',2)
   grid off
   title('Velocity','fontsize',40)
case 2, disp(' Flight Angle ')
   X2 = X2*180/pi;
   plot(X2,'r','linewidth',2)
   grid off
   title('Flight angle (grad)','fontsize',40)
case 3, disp(' Height ')
   X3 = 209*X3;
   plot(X3,'b','linewidth',2)
   grid off
   title('Height (\times 10^5 ft)','fontsize',40)
case 4, disp(' Control in Rad ')
   U = pi*U/180;
   plot(U,'g','linewidth',2)
   grid off
   title('Control (rad)','fontsize',40)
case 5, disp(' Residuum von h ')
   HH = feval('bsp10',X,5,Parmeter);
   plot([1:length(HH)]',HH),hold on
   plot([1:length(HH)]',HH,'k.'),hold on
   title(' Residuum von h ')
   grid on
end
