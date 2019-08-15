function Y = bsp14(X,flag,Parmeter);
% X-38 4 Dgl. simplified
% flag = 1: Objective function
% flag = 2: Inequalities, also []
% flag = 3: Equalities 
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequalities, also []
% flag = 6: Gradient of equalities
% -------------------------------------------
% Gradient von f: R_n -> R_m ist (m,n)-Matrix
% -------------------------------------------
n   = Parmeter(1); BTA  = Parmeter(2); G     = Parmeter(3);
R   = Parmeter(4); RRHO = Parmeter(5); T_END = Parmeter(6);
SDIV2M = Parmeter(7); GG = Parmeter(8);
YAA = Parmeter(9:12); YEE  = Parmeter(13:16);
FAKTOR = [0.5,ones(1,n-1), 0.5]/n; % Gewichte fuer Integration
D      = 0.5*ones(n+1,1);
E      = zeros(n+1,n+1) + tril(ones(n+1,n+1),-1) + diag(D);
E(:,1) = D; E(1,1) = 0;
E      = T_END*sparse(E)/n;

X1  = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
X4  = X(3*n+4:4*(n+1)); U = X(4*n+5:5*(n+1)); % Kontrolle
RHO = RRHO*exp(-BTA*R*X3);
CW  = 0.45; CA = 0.45;
WDIVM   = SDIV2M*RHO.*X1.^2.*CW;
D1WDIVM = 2*SDIV2M*RHO.*X1.*CW;
D3WDIVM = -BTA*R*WDIVM;
ADIVM   = SDIV2M*RHO.*X1.^2.*CA;
D1ADIVM = 2*SDIV2M*RHO.*X1.*CA;
D3ADIVM = -BTA*R*ADIVM;

switch flag
case 1 % T_END nur Faktor, daher weglassen
   Y = FAKTOR*(GG*X1.^3.*sqrt(RHO));
case 3
   COSX2 = cos(X2); SINX2 = sin(X2);
   COSU  = cos(U);  SINU  = sin(U);
   F1 = - WDIVM - G*SINX2;
   F2 =   ADIVM.*COSU./X1 + X1.*COSX2./(R*(1+X3)) - G*COSX2./X1;
   F3 =   X1.*SINX2/R;
   F4 =   ADIVM.*SINU./(X1.*COSX2);
   Y1 =   X1 - YAA(1) - E*F1;
   Y2 =   X2 - YAA(2) - E*F2;
   Y3 =   X3 - YAA(3) - E*F3;
   Y4 =   X4 - YAA(4) - E*F4;
   YA = [Y1;Y2;Y3;Y4(2:n+1)];
   Y  = [YA; X1(n+1) - YEE(1); X3(n+1) - YEE(3)];
case 4
   F1 =    GG*3*X1.^2.*sqrt(RHO);
   F3 =  - GG*BTA*R*X1.^3.*sqrt(RHO)/2;
   Y  =    [FAKTOR.*F1',zeros(1,n+1),FAKTOR.*F3',zeros(1,2*(n+1))];
case 6
   COSX2 = cos(X2); SINX2 = sin(X2);
   COSU  = cos(U);  SINU  = sin(U);
   RX3   = R*(1 + X3);

   D1F1 = - D1WDIVM;  D2F1 = - G*COSX2;  D3F1 = - D3WDIVM;

   D1F2 =   D1ADIVM.*COSU./X1 - ADIVM.*COSU./X1.^2 ...
          + COSX2./RX3 + G*COSX2./X1.^2;
   D2F2 = - X1.*SINX2./RX3 + G*SINX2./X1;
   D3F2 =   D3ADIVM.*COSU./X1 - R*X1.*COSX2./RX3.^2;
   DUF2 = - ADIVM.*SINU./X1;

   D1F3 =   SINX2/R;
   D2F3 =   X1.*COSX2/R;

   D1F4 = D1ADIVM.*SINU./(X1.*COSX2) - ADIVM.*SINU./(X1.^2.*COSX2);
   D2F4 = ADIVM.*SINU.*SINX2./(X1.*COSX2.^2);
   D3F4 = D3ADIVM.*SINU./(X1.*COSX2);
   DUF4 = ADIVM.*COSU./(X1.*COSX2);
   Y1 = [eye(n+1)-E*diag(D1F1), - E*diag(D2F1), ...
         -E*diag(D3F1),         zeros(n+1,2*(n+1))];
   Y2 = [-E*diag(D1F2), eye(n+1) - E*diag(D2F2), ...
         -E*diag(D3F2),            zeros(n+1,n+1), ...
         -E*diag(DUF2)];
   Y3 = [-E*diag(D1F3),          - E*diag(D2F3), ...
          eye(n+1),                zeros(n+1,2*(n+1))];
   Y4 = [- E*diag(D1F4),           - E*diag(D2F4), ...
         - E*diag(D3F4),           eye(n+1),...
         - E*diag(DUF4)];
   YA = [Y1;Y2;Y3;Y4(2:n+1,:)];
   YB = zeros(2,5*(n+1));
   YB(1,n+1)     = 1;
   YB(2,3*(n+1)) = 1;

   Y             = [YA;YB];
end
