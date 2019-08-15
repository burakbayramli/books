function WN = vorticity1(p,t,WBA,W,Z,Parmeter,RSIDE)
% Computation of vorticity

DT = Parmeter(1); %  time step
NU = Parmeter(2); %  coefficient of viscosity
X  = p(1,:); Y = p(2,:);
M  = size(t,2); N = length(X);
WN = zeros(N,1); V = zeros(M,2); FLAECHE = zeros(M,1);
MMM  = sparse(N,N); KKK = sparse(N,N);
B1   = zeros(N,1); B2 = B1;
for I = 1:M
   K   = t(1:3,I); WW = W(K);
   X21 = X(K(2))-X(K(1)); X31 = X(K(3))-X(K(1)); X32 = X(K(3))-X(K(2));
   Y21 = Y(K(2))-Y(K(1)); Y31 = Y(K(3))-Y(K(1)); Y32 = Y(K(3))-Y(K(2));
   Z21 = Z(K(2))-Z(K(1)); Z31 = Z(K(3))-Z(K(1)); Z32 = Z(K(3))-Z(K(2));
   FLAECHE(I) = (X21*Y31 - X31*Y21)/2;
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   V(I,1) =   0.5*(X32*Z(K(1))-X31*Z(K(2))+X21*Z(K(3)))/FLAECHE(I);
   V(I,2) = - 0.5*(-Y32*Z(K(1))+Y31*Z(K(2))-Y21*Z(K(3)))/FLAECHE(I);
   W_AUX  = (-Z32*WW(1) + Z31*WW(2) - Z21*WW(3))/6; % quadrat. Anteil
   NUX = NU + 0.5*V(I,1)*V(I,1)*DT;  % art. viscosity in x-Richtung
   NUY = NU + 0.5*V(I,2)*V(I,2)*DT;  % art. viscosity in y-Richtung
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   MM = FLAECHE(I)*[2, 1, 1; 1, 2, 1; 1, 1, 2]/12; % Massenmatrix
   KK = zeros(3,3);  % Steifigkeitsmatrix
   KK(1,1) =   X32*X32*NUY + Y32*Y32*NUX;  % O.K.
   KK(1,2) = - X32*X31*NUY - Y32*Y31*NUX;
   KK(1,3) =   X32*X21*NUY + Y32*Y21*NUX;
   KK(2,2) =   X31*X31*NUY + Y31*Y31*NUX;
   KK(2,3) = - X31*X21*NUY - Y31*Y21*NUX;
   KK(3,3) =   X21*X21*NUY + Y21*Y21*NUX;
   KK(2,1) = KK(1,2); KK(3,1) = KK(1,3); KK(3,2) = KK(2,3);
   KKK(K,K)= KKK(K,K) + KK/(4*FLAECHE(I));
   MMM(K,K)= MMM(K,K) + MM;
   B1(K)   = B1(K) + MM*WW;
   B2(K)   = B2(K) - W_AUX;
   %B2(K)   = B2(K) - W_AUX + MM*RSIDE(K)';

end
B2 = B2 + MMM*RSIDE.';
A = MMM + DT*KKK; A = sparse(A); B = B1 + DT*B2;
% -- Dirichlet-Randbedingung ---------------
J      = WBA(1,:);
B      = B - A(:,J)*WBA(2,:)';
B(J)   = WBA(2,:)';
A(J,:) = 0; A(:,J) = 0;
for I = 1:size(WBA,2), J = WBA(1,I); A(J,J)  = 1; end
% -- solving system for vorticity
WN = A\B;
