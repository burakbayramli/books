function myquivd(X,Y,c,d,farbe,lwidth,ltyp,pfeiltyp)

% Zeichnet Doppelpfeil
% c            : Laenge der Pfeilspitze
% d            : halbe Breite der Pfeilspitze
% X(:,I)       : Anfangspunkt des I-ten Pfeils
% Y(:,I)       : Endpunkt des I-ten Pfeils
% pfeiltyp = 1 : Zugstab
% pfeiltyp = 2 : Druckstab

N = size(X,2);
for I = 1:N
   A = X(:,I);
   B = Y(:,I);
   W = [A, B];
   if ltyp == 1
      plot(W(1,:),W(2,:),'Linewidth',lwidth,'Color',farbe)
   end
   if ltyp == 2
      plot(W(1,:),W(2,:),'--','Linewidth',lwidth,'Color',farbe)
   end
   if ltyp == 3
      plot(W(1,:),W(2,:),':','Linewidth',lwidth,'Color',farbe)
   end
end
hold on
% Zeichne Pfeilspitzen --------------
for I = 1:N
   if pfeiltyp == 1
      A = X(:,I);
      B = Y(:,I);
      U = B - A;
      if norm(U) ~= 0
         U = U/norm(U);
         V = [-U(2); U(1)];
         C = B - c*U;
         D = B - d*V;
         E = B + d*V;
         Z = [C, D, E, C];
         fill(Z(1,:),Z(2,:),farbe);
         hold on
         C = A + c*U;
         D = A + d*V;
         E = A - d*V;
         Z = [C, D, E, C];
         fill(Z(1,:),Z(2,:),farbe);
         hold on
      end
   end
   if pfeiltyp == 2
      A = X(:,I);
      B = Y(:,I);
      U = B - A;
      if norm(U) ~= 0
         U = U/norm(U);
         V = [-U(2); U(1)];
         C = B - c*U + d*V;
         D = B - c*U - d*V;
         Z = [B, C, D, B];
         fill(Z(1,:),Z(2,:),farbe);
         hold on
         C = A + c*U - d*V;
         D = A + c*U + d*V;
         Z = [A, C, D, A];
         fill(Z(1,:),Z(2,:),farbe);
         hold on
      end
   end
end
