function Y = bsp03(flag,mue,X,Parmeter);
% Small Brusselator
% omega*u' + A*u + mue*B*u + f(mue,u) = 0
% omega*u' + h(mue,u) = 0
% flag = 1: A, B, Re_c, Im_c, Re_d, Im_d, omga0
% flag = 2: Nonlinear part f
% flag = 3: Function h for continuation
% flag = 4: Gradient(h)*V for continuation
% flag = 5: Gradient^T(h)*V for continuation
% -------------------------------------------
% Gradient of f: R_n -> R_m is (m,n)-matrix
% -------------------------------------------
a = 1;
switch flag
case 1
   omga0 = a;
   AUX   = [omga0; 0];
   A = [-a*a   -a*a; a*a+1  a*a];
   B = [  -1      0;     1    0];
   Re_c = zeros(2,1);
   Re_c(2) = sqrt(2*(a*a + 1)/(1 + 2*a*a));
   Re_c(1) = - a*a*Re_c(2)/(1 + a*a);
   Im_c = [- a*Re_c(2)/(1 + a*a); 0];
   Re_d = [0  ; 1/Re_c(2)];
   Im_d = [- (a*a + 1)/(a*Re_c(2)); - a/Re_c(2)];
   Y = [A, B, Re_c, Im_c, Re_d, Im_d, AUX];
   % Testen der Eigenvektoren
   % c = Re_c + sqrt(-1)*Im_c; d = Re_d + sqrt(-1)*Im_d;
   % DIFF1 = norm(A*c + sqrt(-1)*omga0*c);
   % DIFF2 = norm((A')*d - sqrt(-1)*omga0*d);
   % c_Hc = c'*c; d_Hc = d'*c; d_Tc = norm(d.'*c);
   % DIFF1_DIFF2    = [DIFF1,DIFF2]
   % c_Hc_d_Hc_d_Tc = [c_Hc,d_Hc,d_Tc]
   % pause
case 2, Y = X;
   Y(1,:) = - (a + mue/a + 1/a)*X(1,:).^2 - 2*a*X(1,:).*X(2,:) - X(1,:).^2.*X(2,:);
   Y(2,:) = (a + mue/a + 1/a)*X(1,:).^2 + 2*a*X(1,:).*X(2,:) + X(1,:).^2.*X(2,:);
case 3, Y = X;
   a1 = a^2 + mue; a2  = a + mue/a + 1/a; a3  = a^2+1+mue;
   XX     = X(1,:).*X(1,:);
      Y(1,:) = -a1*X(1,:)  - a^2*X(2,:);
      Y(1,:) = Y(1,:) - a2*XX - 2*a*X(1,:).*X(2,:) - XX.*X(2,:);
      Y(2,:) =  a3*X(1,:) + a^2*X(2,:);
      Y(2,:) = Y(2,:) + a2*XX + 2*a*X(1,:).*X(2,:) + XX.*X(2,:);
case 4
   Y   = X(1:2,:);
   XX  = X(1,:).*X(1,:);
   a1  = a^2 + mue; a2  = a + mue/a + 1/a; a3  = a^2+1+mue;
   Y11 = -a1 - 2*a2*X(1,:) - 2*a*X(2,:) - 2*X(1,:).*X(2,:);
   Y12 = -a^2 - 2*a*X(1,:) - XX;
   Y21 = a3 + 2*a2*X(1,:) + 2*a*X(2,:) + 2*X(1,:).*X(2,:);
   Y22 = a^2 + 2*a*X(1,:) + XX;
   Y(1,:) = Y11.*X(3,:) + Y12.*X(4,:);
   Y(2,:) = Y21.*X(3,:) + Y22.*X(4,:);
case 5
   Y   = X(1:2,:);
   XX  = X(1,:).*X(1,:);
   a1  = a^2 + mue; a2 = a + mue/a + 1/a; a3  = a^2+1+mue;
   Y11 = -a1 - 2*a2*X(1,:) - 2*a*X(2,:) - 2*X(1,:).*X(2,:);
   Y12 = -a^2 - 2*a*X(1,:) - XX;
   Y21 = a3 + 2*a2*X(1,:) + 2*a*X(2,:) + 2*X(1,:).*X(2,:);
   Y22 = a^2 + 2*a*X(1,:) + XX;
   Y(1,:) = Y11.*X(3,:) + Y21.*X(4,:);
   Y(2,:) = Y12.*X(3,:) + Y22.*X(4,:);
end
