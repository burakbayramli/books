function myquiver(X,Y,c,d,farbe,lwidth,TYP)

% Zeichnet Pfeil
% c : Laenge der Pfeilspitze
% d : Breite der Pfeilspitze
% X(:,I) : Anfangspunkt des I-ten Pfeils
% Y(:,I) : Endpunkt des I-ten Pfeils

N = size(X,2);
for I = 1:N
      A = X(:,I);
      B = Y(:,I);
      U = B - A;
      if norm(U) ~= 0
      U = U/norm(U);
      V = [-U(2); U(1)];
      W = [A, B];
      C = B - c*U + d*V;
      D = B - c*U - d*V;
      Z = [B, C, D, B];
      if TYP == 1
         plot(W(1,:),W(2,:),'Linewidth',lwidth,'Color',farbe)
      end
      if TYP == 2
         plot(W(1,:),W(2,:),'--','Linewidth',lwidth,'Color',farbe)
      end
      if TYP == 3
         plot(W(1,:),W(2,:),':','Linewidth',lwidth,'Color',farbe)
      end
      hold on
      fill(Z(1,:),Z(2,:),farbe);
      hold on
   end
end
