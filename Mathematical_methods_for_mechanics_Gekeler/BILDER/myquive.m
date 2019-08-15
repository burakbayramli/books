function myquive(X,Y,Z,c,d,farbe,lwidth,ltyp)

% Zeichnet Kegel
% c            : Laenge des Kegels
% d            : Radius der Kegelbasis
% X            : X-Koordinaten des Pfeils
% Y            : Y-Koordinaten des Pfeils
% Z            : Z-Koordinaten des Pfeils

N = 20;
A = [X(1);Y(1);Z(1)];
B = [X(2);Y(2);Z(2)];
U = B - A;
if norm(U) ~= 0
   U = U/norm(U);
   if U(1) == 0
      V = [0;-U(3);U(2)];
   else
      V = [-U(3);0;U(1)];
   end
   W = [U(2)*V(3) - U(3)*V(2);
        U(3)*V(1) - U(1)*V(3);
        U(1)*V(2) - U(2)*V(1)];
   C = B - c*U;
   T = linspace(0,2*pi,N);
   Z = (B - c*U)*ones(1,N) + d*V*cos(T)+ d*W*sin(T);
   plot3(Z(1,:),Z(2,:),Z(3,:)), hold on
   fill3(Z(1,:),Z(2,:),Z(3,:),'w'), hold on
   X1 = [A(1),C(1)]; Y1 = [A(2),C(2)]; Z1 = [A(3),C(3)];
   if ltyp == 1
      plot3(X1,Y1,Z1,'Linewidth',lwidth,'Color',farbe)
   end
   if ltyp == 2
      plot3(X1,Y1,Z1,'--','Linewidth',lwidth,'Color',farbe)
   end
   if ltyp == 3
      plot3(X1,Y1,Z1,':','Linewidth',lwidth,'Color',farbe)
   end
   hold on
   plot3([A(1),C(1)],[A(2),C(2)],[A(3),C(3)],'k','linewidth',2)
   hold on
   for I = 1:N
       U = [Z(:,I),B];
       plot3(U(1,:),U(2,:),U(3,:),'g','linewidth',2), hold on
   end
end
