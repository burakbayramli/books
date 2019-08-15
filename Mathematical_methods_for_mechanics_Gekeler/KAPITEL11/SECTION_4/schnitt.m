function [Y,errorcode] = schnitt(M,R,A,B)
% Schnittpunkt eines Kreises mit MP M, Radius R
% und STRECKE X = A + LAMBDA*(B - A)
errorcode = 0; Y = [0;0];
alfa = (B(1) - A(1))^2 + (B(2) - A(2))^2;
if alfa == 0 % A = B in Schnitt
   errorcode = 1; return
end
if alfa ~= 0
   beta = (A(1)-M(1))*(B(1)-A(1)) + (A(2)-M(2))*(B(2)-A(2));
   gama = (A(1) - M(1))^2 + (A(2) - M(2))^2 - R*R;
   diskrim = beta*beta - alfa*gama;
   if diskrim < 0 %komplexe Wurzeln: kein Schnittpunkt
      errorcode = 2; return
   else
      LAMBDA = (-beta + sqrt(diskrim))/alfa;
      if abs(LAMBDA) > 1
         LAMBDA = (-beta - sqrt(diskrim))/alfa;
      end
      if LAMBDA >= 0 & LAMBDA <= 1
         Y = A + LAMBDA*(B - A);
      else % Schnittpunkt ausserhalb Strecke
         errorcode = 3; return
      end
   end
end
