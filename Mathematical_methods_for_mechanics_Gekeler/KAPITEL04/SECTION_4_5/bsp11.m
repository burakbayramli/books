function Y = bsp11(X,flag,Parmeter);
% Reentry-Problem, Stoer, p. 491
% flag = 1: Objective function
% flag = 2: Inequalities, also []
% flag = 3: Equalities 
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequalities, also []
% flag = 6: Gradient of equalities
% -------------------------------------------
% !! Gradient of f: R_n -> R_m is (m,n)-matrix !!
% -------------------------------------------
n  = Parmeter(1);   BTA  = Parmeter(2); G     = Parmeter(3);
R  = Parmeter(4);   RRHO = Parmeter(5); T_END = Parmeter(6);
YA = Parmeter(7:9); YE   = Parmeter(10:12);
SDIV2M = Parmeter(13); GG = Parmeter(14);
TRAPEZRULE = [0.5,ones(1,n-1), 0.5]/n; % weights of trapez rule
D      = 0.5*ones(n+1,1);
E      = zeros(n+1,n+1) + tril(ones(n+1,n+1),-1) + diag(D);
E(:,1) = D; E(1,1) = 0;
E      = T_END*sparse(E)/n; % trapez.-rule for constraint

X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
U  = X(3*n+4:4*(n+1)); % Kontrolle
RHO  = RRHO*exp(-BTA*R*X3);
Q    = SDIV2M*RHO;
CW  = 1.174 - 0.9*cos(U);
CA  = 0.6*sin(U);
switch flag
case 1
   F1 = GG*X1.^3.*sqrt(RHO);
   Y  = TRAPEZRULE*F1;
case 2, Y = [U + pi;pi - U]; Y = 10*Y;
case 3
   F1 = - Q.*X1.^2.*CW + G*sin(X2);
   F2 =   Q.*X1.*CA + X1.*cos(X2)./(R*(1+X3)) - G*cos(X2)./X1;
   F3 =   X1.*sin(X2)/R;
   Y = [X1 - YA(1) - E*F1;
        X2 - YA(2) - E*F2;
        X3 - YA(3) - E*F3;
        X1(n+1) - YE(1);
        X2(n+1) - YE(2);
        X3(n+1) - YE(3)];
case 4
   F1 =  GG*3*X1.^2.*sqrt(RHO);
   F3 =  - GG*BTA*R*X1.^3.*sqrt(RHO)/2;
   Y  = [TRAPEZRULE.*F1',zeros(1,n+1),TRAPEZRULE.*F3',zeros(1,n+1)];
case 5,
   Y = [zeros(n+1,3*(n+1)), eye(n+1);
        zeros(n+1,3*(n+1)), -eye(n+1)]; Y = 10*Y;
case 6
   D1F1 = - 2*Q.*X1.*CW;
   D2F1 = - G*cos(X2);
   D3F1 =   BTA*R*Q.*X1.^2.*CW;
   D4F1 = - 0.9*Q.*X1.^2.*sin(U);
   D1F2 =   Q.*CA + cos(X2)./(R*(1 + X3)) + G*cos(X2)./X1.^2;
   D2F2 = - X1.*sin(X2)./(R*(1+X3)) + G*sin(X2)./X1;
   D3F2 = - BTA*R*Q.*X1.*CA - X1.*cos(X2)./(R*(1+X3).^2);
   D4F2 =   0.6*Q.*X1.*cos(U);
   D1F3 =   sin(X2)/R;
   D2F3 =   X1.*cos(X2)/R;
   D3F3 =   zeros(n+1,1);
   D4F3 =   zeros(n+1,1);
   Y1 = [eye(n+1)-E*diag(D1F1),         - E*diag(D2F1), ...
                 -E*diag(D3F1),         - E*diag(D4F1);
                - E*diag(D1F2), eye(n+1)-E*diag(D2F2), ...
                 -E*diag(D3F2),       - E*diag(D4F2);
                - E*diag(D1F3),       - E*diag(D2F3), ...
         eye(n+1)-E*diag(D3F3),       - E*diag(D4F3)];

   Y2 = zeros(3,4*(n+1));
   Y2(1,n+1)     = 1;
   Y2(2,2*(n+1)) = 1;
   Y2(3,3*(n+1)) = 1;
   Y             = [Y1;Y2];
end
