function Y = bsp15(X,flag,Parmeter)
% Raumgleiter X-38, skalierte Dglen
% no constraint on flight path angle
% flag = 1: Objective function
% flag = 2: Inequalities, also []
% flag = 3: Equalities 
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequalities, also []
% flag = 6: Gradient of equalities
% -------------------------------------------
% !! Gradient of f: R_n -> R_m is (m,n)-matrix !!
% -------------------------------------------
Parmtr1 = Parmeter(1:9);
Parmtr2 = Parmeter(10:27);
n   = Parmtr1(1);   BTA  = Parmtr1(2);  G     = Parmtr1(3);
R   = Parmtr1(4);   RRHO = Parmtr1(5);  T_END = Parmtr1(6);
SDIV2M = Parmtr1(7); GG = Parmtr1(8); omga = Parmtr1(9);
CW_DAT = Parmtr2(1:3); CA_DAT = Parmtr2(4:6);
YAA = Parmtr2(7:12); YEE = Parmtr2(13:18);
FAKTOR = [0.5,ones(1,n-1),0.5]/n;  % Gewichte fuer Integration
D      = 0.5*ones(n+1,1);
E      = zeros(n+1,n+1) + tril(ones(n+1,n+1),-1) + diag(D);
E(:,1) = D; E(1,1) = 0;
E      = T_END*sparse(E)/n;
X1  = X(1:n+1);         X2 = X(n+2:2*(n+1));   X3 = X(2*n+3:3*(n+1));
X4  = X(3*n+4:4*(n+1)); X5 = X(4*n+5:5*(n+1));
X6 = X(5*n+6:6*(n+1));  U = X(6*n+7:7*(n+1));

CW  = CW_DAT(1) + CW_DAT(2)*10^5*X1 + CW_DAT(3)*10^10*X1.*X1;
CA  = CA_DAT(1) + CA_DAT(2)*10^5*X1 + CA_DAT(3)*10^10*X1.*X1;

CWX = CW_DAT(2)*10^5 + 2*CW_DAT(3)*10^10*X1;
CAX = CA_DAT(2)*10^5 + 2*CA_DAT(3)*10^10*X1;


%CW = 0.5; CA = 0.5;
RHO     =   RRHO*exp(-BTA*R*X3);
WDIVM   =   SDIV2M*CW.*RHO.*X1.^2;
D1WDIVM =   SDIV2M*RHO.*(2*CW.*X1 + CWX.*X1.^2);
%D1WDIVM =   2*CW*SDIV2M*RHO.*X1;

D3WDIVM = - BTA*R*WDIVM;
ADIVM   =   SDIV2M*CA.*RHO.*X1.^2;
D1ADIVM =   SDIV2M*RHO.*(2*CA.*X1 + CAX.*X1.^2);
%D1ADIVM =   2*CA*SDIV2M*RHO.*X1;
D3ADIVM = - BTA*R*ADIVM;

GGX2 = 20;   % zusaetzliches Gewicht fuer X2
             % GGX2 = 40 bei n = 150 gleiches Ergebnis
switch flag
case 1
   Y = GG*FAKTOR*(real(X1.^3.15).*sqrt(RHO));
case 2, Y = [];
   case 3
   COSX2   = cos(X2); SINX2 = sin(X2);
   COSX4   = cos(X4); SINX4 = sin(X4);
   COSX5   = cos(X5); SINX5 = sin(X5);
   COSU    = cos(U);  SINU = sin(U);
   TANX2   = tan(X2); TANX5 = tan(X5);
   RX3     = R*(1 + X3);
   F1  = - WDIVM - G*SINX2;
   F2A =   ADIVM.*COSU./X1 + X1.*COSX2./RX3;
   F2B = - G*COSX2./X1 + 2*omga*COSX4.*SINX5;
   F2  =   F2A + F2B;
   F3  =   X1.*SINX2/R;
   F4A =   ADIVM.*SINU./(X1.*COSX2);
   F4B =   X1.*COSX2.*SINX4.*TANX5./RX3;
   F4C = - 2*omga*TANX2.*COSX4.*COSX5 + 2*omga*SINX5;
   F4  =   F4A + F4B + F4C;
   F5  =   X1.*COSX2.*COSX4./RX3;
   F6  =   X1.*COSX2.*SINX4./(RX3.*COSX5);
   Y1  =   X1 - YAA(1) - E*F1;
   Y2  =   X2 - YAA(2) - E*F2;
   Y3  =   X3 - YAA(3) - E*F3;
   Y4  =   X4 - YAA(4) - E*F4;
   Y5  =   X5 - YAA(5) - E*F5;
   Y6  =   X6 - YAA(6) - E*F6;

   YA  =   [Y1;GGX2*Y2;Y3;Y4;Y5;Y6];
   YB  =   [X1(n+1) - YEE(1); X3(n+1) - YEE(3);
            X5(n+1) - YEE(5); X6(n+1) - YEE(6)];
   Y   =   [YA;YB];
case 4
   F1 =   GG*3.15*real(X1.^2.15).*sqrt(RHO);
   F3 = - GG*BTA*R*real(X1.^3.15).*sqrt(RHO)/2;
   Y =    [FAKTOR.*F1',zeros(1,n+1),FAKTOR.*F3',zeros(1,4*(n+1))];
case 5, Y = [];
case 6
   COSX2   = cos(X2); SINX2 = sin(X2);
   COSX4   = cos(X4); SINX4 = sin(X4);
   COSX5   = cos(X5); SINX5 = sin(X5);
   COSU    = cos(U);  SINU = sin(U);
   TANX2   = tan(X2); TANX5 = tan(X5);
   RX3     = R*(1 + X3);

   D1F1  = - D1WDIVM; D2F1 = -G*COSX2; D3F1 = - D3WDIVM;
   D1F2A =   D1ADIVM.*COSU./X1 - ADIVM.*COSU./X1.^2;
   D1F2B =   COSX2./RX3 + G*COSX2./X1.^2;
   D1F2  =   D1F2A + D1F2B;
   D2F2  = - X1.*SINX2./RX3 + G*SINX2./X1;
   D3F2  =   D3ADIVM.*COSU./X1 - R*X1.*COSX2./RX3.^2;
   D4F2  =   2*omga*COSX4.*COSX5;
   D5F2  = - 2*omga*SINX4.*SINX5;
   DUF2  = - ADIVM.*SINU./X1;
   D1F3  =   SINX2/R; D2F3 = X1.*COSX2/R;

   D1F4A =   D1ADIVM.*SINU./(X1.*COSX2);
   D1F4B = - ADIVM.*SINU./(X1.^2.*COSX2);
   D1F4C =   COSX2.*SINX4.*TANX5./RX3;
   D1F4  =   D1F4A + D1F4B + D1F4C;

   D2F4A =   ADIVM.*TANX2.*SINU./(X1.*COSX2);
   D2F4B = - X1.*SINX2.*SINX4.*TANX5./RX3;
   D2F4C = - 2*omga*COSX4.*COSX5./COSX2.^2;
   D2F4  =   D2F4A + D2F4B + D2F4C;

   D3F4A =   D3ADIVM.*SINU./(X1.*COSX2);
   D3F4B = - R*X1.*COSX2.*SINX4.*TANX5./RX3.^2;
   D3F4  =   D3F4A + D3F4B;

   D4F4A =   X1.*COSX2.*COSX4.*TANX5./RX3;
   D4F4B =   2*omga*TANX2.*SINX4.*COSX5;
   D4F4  =   D4F4A + D4F4B;

   D5F4A =   X1.*COSX2.*SINX4./(RX3.*COSX5.^2);
   D5F4B =   2*omga*TANX2.*COSX4.*SINX5;
   D5F4C =   2*omga*COSX5;
   D5F4  =   D5F4A + D5F4B + D5F4C;

   DUF4  =   ADIVM.*COSU./(X1.*COSX2);

   D1F5  =   COSX2.*COSX4./RX3;
   D2F5  = - X1.*SINX2.*COSX4./RX3;
   D3F5  = - R*X1.*COSX2.*COSX4./RX3.^2;
   D4F5  = - X1.*COSX2.*SINX4./RX3;

   D1F6  =   COSX2.*SINX4./(RX3.*COSX5);
   D2F6  = - X1.*SINX2.*SINX4./(RX3.*COSX5);
   D3F6  = - R*X1.*COSX2.*SINX4./(RX3.^2.*COSX5);
   D4F6  =   X1.*COSX2.*COSX4./(RX3.*COSX5);
   D5F6  =   X1.*COSX2.*SINX4.*SINX5./(RX3.*COSX5.^2);

   Y1 = [eye(n+1)-E*diag(D1F1), -E*diag(D2F1), -E*diag(D3F1), ...
         zeros(n+1,4*(n+1))];
   Y2 = [-E*diag(D1F2), eye(n+1)-E*diag(D2F2), ...
         -E*diag(D3F2), - E*diag(D4F2), ...
         -E*diag(D5F2), zeros(n+1,n+1), -E*diag(DUF2)];
   Y3 = [-E*diag(D1F3), -E*diag(D2F3), eye(n+1), ...
          zeros(n+1,4*(n+1))];
   Y4 = [-E*diag(D1F4), -E*diag(D2F4), -E*diag(D3F4),...
          eye(n+1)-E*diag(D4F4),...
          -E*diag(D5F4), zeros(n+1,n+1), -E*diag(DUF4)];
   Y5 = [-E*diag(D1F5), -E*diag(D2F5), ...
        - E*diag(D3F5), -E*diag(D4F5), ...
          eye(n+1), zeros(n+1,2*(n+1))];
   Y6 = [-E*diag(D1F6), -E*diag(D2F6), ...
        - E*diag(D3F6), -E*diag(D4F6), ...
        - E*diag(D5F6)   eye(n+1), zeros(n+1,n+1)];


   YA = [Y1;GGX2*Y2;Y3;Y4;Y5;Y6];
   YB =  zeros(4,7*(n+1));
   YB(1,n+1) = 1;     YB(2,3*(n+1)) = 1;
   YB(3,5*(n+1)) = 1; YB(4,6*(n+1)) = 1;

   Y = [YA;YB]; Y = sparse(Y);
end
