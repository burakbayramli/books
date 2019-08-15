function TN = convection_r(p,t,Z,RDT,RCT,RDM,T,Parmeter)
DT = Parmeter(1);
NU = Parmeter(2); LAMDA = Parmeter(3);
KAPPA = Parmeter(5); TAIR = Parmeter(6);

X = p(1,:); Y = p(2,:);
M = size(t,2); N = length(X); V = zeros(M,2);
A  = sparse(N,N); B = zeros(N,1);
for I = 1:M
   K      = t(1:3,I);
   X21 = X(K(2))-X(K(1)); X31 = X(K(3))-X(K(1)); X32 = X(K(3))-X(K(2));
   Y21 = Y(K(2))-Y(K(1)); Y31 = Y(K(3))-Y(K(1)); Y32 = Y(K(3))-Y(K(2));
   Z21 = Z(K(2))-Z(K(1)); Z31 = Z(K(3))-Z(K(1)); Z32 = Z(K(3))-Z(K(2));
   FLAECHE = (X21*Y31 - X31*Y21)/2;
   if I == RDM(1)
      FL_SMOKE = FLAECHE;
   end   
   % -- Velocity -------------------------------------
   V(I,1) =   0.5*(X32*Z(K(1))-X31*Z(K(2))+X21*Z(K(3)))/FLAECHE;
   V(I,2) = - 0.5*(-Y32*Z(K(1))+Y31*Z(K(2))-Y21*Z(K(3)))/FLAECHE;
   T_AUX  = (-Z32*T(K(1)) + Z31*T(K(2)) - Z21*T(K(3)))/6; % quadrat. Anteil
   NUX = LAMDA + 0.5*V(I,1)*V(I,1)*DT;  % art. viscosity in x-Richtung
   NUY = LAMDA + 0.5*V(I,2)*V(I,2)*DT;  % art. viscosity in y-Richtung

   MM = FLAECHE*[2, 1, 1; 1, 2, 1; 1, 1, 2]/12; % Massenmatrix
   KK = zeros(3,3);                           % Steifigkeitsmatrix
   KK(1,1) =   X32*X32*NUY + Y32*Y32*NUX;
   KK(1,2) = - X32*X31*NUY - Y32*Y31*NUX;
   KK(1,3) =   X32*X21*NUY + Y32*Y21*NUX;
   KK(2,2) =   X31*X31*NUY + Y31*Y31*NUX;
   KK(2,3) = - X31*X21*NUY - Y31*Y21*NUX;
   KK(3,3) =   X21*X21*NUY + Y21*Y21*NUX;
   KK(2,1) = KK(1,2); KK(3,1) = KK(1,3); KK(3,2) = KK(2,3);
   KK      = KK/(4*FLAECHE);
   A_AUX  = MM/DT + KK;
   B_AUX  = MM*T(K)/DT - T_AUX;
   %%% AENDERUNG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %B_AUX = B_AUX + MM*(- DELTA_T(K) + LAMDA*CONVEC_T(K));
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   A(K,K) = A(K,K) + A_AUX;
   B(K)   = B(K)   + B_AUX;
end
%% -- Cauchy boundary conditions of temperature ---
if ~isempty(RCT)
   for I = 1:size(RCT,2)
      K       = RCT(1:2,I);
      [ME,BE] = ralell(p(1,K),p(2,K));
      B(K)    = B(K) - KAPPA*(ME*T(K) - TAIR*BE);
   end
end
% -- smoke orign ----------------------------
if ~isempty(RDM)
   KQ = RDM(1); PPB = RDM(2);
   SSS = 2*FL_SMOKE*PPB/6;             % mal DT ?
   J = t(:,KQ); B(J) = B(J) + SSS;
end
% -- boundary condition for Polution
J      = RDT(1,:);
B      = B - A(:,J)*RDT(2,:)';
B(J)   = RDT(2,:)';
A(J,:) = 0; A(:,J) = 0;
for I = 1:size(RDT,2)
   J = RDT(1,I); A(J,J)  = 1;
end
% -- solving system for temperature -----
TN = A\B;
