function Y = bsp01(flag,mue,X,Parmeter);
% Van der Pol equation
% omega*u' + A*u + mue*B*u + f(mue,u) = 0
% omega*u' + h(mue,u) = 0
% flag = 1: A, B, Re_c, Im_c, Re_d, Im_d, omga0
% flag = 2: Nonlinear part f
% flag = 3: Funktion h for continuation
% flag = 4: Gradient(h)*V for continuation
% flag = 5: Gradient^T(h)*V for continuation
% -------------------------------------------
% Gradient of f: R_n -> R_m is (m,n)-matrix
% -------------------------------------------
switch flag
case 1
   omga0 = 1; AUX   = [omga0; 0];
   A     = [0  -1; 1   0];
   B     = [0   0; 0  -1];
   Re_c  = [1; 0]; Im_c  = [0; 1];
   Re_d  = [1; 0]; Im_d  = [0; 1];
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
    Y(1,:) = zeros(1,size(X,2)); Y(2,:) = X(1,:).^2.*X(2,:);
case 3, Y = X;
    Y(1,:) = - X(2,:);
    Y(2,:) =   X(1,:) - mue*X(2,:) + X(1,:).^2.*X(2,:);
case 4, Y = X(1:2,:);
   Y11 = 0;                    Y12 = - 1;
   Y21 = 1 + 2*X(1,:).*X(2,:); Y22 = X(1,:).*X(1,:) - mue;
   Y(1,:) = Y11.*X(3,:) + Y12.*X(4,:);
   Y(2,:) = Y21.*X(3,:) + Y22.*X(4,:);
case 5, Y = X(1:2,:);
   Y11 = 0;                    Y12 = - 1;
   Y21 = 1 + 2*X(1,:).*X(2,:); Y22 = X(1,:).*X(1,:) - mue;
   Y(1,:) = Y11.*X(3,:) + Y21.*X(4,:);
   Y(2,:) = Y12.*X(3,:) + Y22.*X(4,:);
end
