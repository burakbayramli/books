function Y = bsp04(flag,mue,X,Parmeter);
% Full Brusselator
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
a0 = Parmeter(1);
Y0 = Parmeter(2:4);
A     = [1-a0, -1, 1; a0, 1, -1; a0, 0, 1];
B     = [-1, 0, 0; 1, 0, 0; 1, 0, 0];
switch flag
case 1
   [V,D] = eig(A); omga0 = imag(D(1,1));
   c     = V(:,2); c     = sqrt(2)*c/norm(c);
   Re_c  = real(c); Im_c  = imag(c);
   [V,D] = eig(A.');
   d     = V(:,1); d     = sqrt(2)*d/norm(d);
   d     = 2*d/(0.0496+1.2454i);
   Re_d  = real(d); Im_d  = imag(d);
   AUX   = [omga0; 0; 0];
   Y = [A, B, Re_c, Im_c, Re_d, Im_d, AUX];
   % Testen der Eigenvektoren
   % DIFF1 = norm(A*c + sqrt(-1)*omga0*c);
   % DIFF2 = norm((A.')*d - sqrt(-1)*omga0*d);
   % c_Hc = c'*c; d_Hc = d'*c; d_Tc = norm(d.'*c);
   % DIFF1_DIFF2    = [DIFF1,DIFF2]
   % c_Hc_d_Hc_d_Tc = [c_Hc,d_Hc,d_Tc]
   % pause
case 2
   a1 = a0 + mue;
   Y = X;
   AUX  =   a1*X(1,:).^2 + 2*X(1,:).*X(2,:) + X(1,:).^2.*X(2) - X(1,:).*X(3,:);
   Y(1,:) = - AUX;
   Y(2,:) =   AUX;
   Y(3,:) =  X(1,:).*X(3,:);
case 3
   a1     = a0 + mue;
   A      = [1-a1, -1, 1; a1, 1, -1; a1, 0, 1];
   Y      = X;
   AUX    = a1*X(1,:).^2 + 2*X(1,:).*X(2,:) + X(1,:).^2.*X(2,:) - X(1,:).*X(3,:);
   Y(1,:) = - AUX;
   Y(2,:) =   AUX;
   Y(3,:) =  X(1,:).*X(3,:);
   Y = A*X + Y;
case 4
   a1 = a0 + mue;
   Y = X(1:3,:);
   AUX_1 = 2*a1*X(1,:) + 2*X(2,:) + 2*X(1,:).*X(2,:) - X(3,:);
   AUX_2 = 2*X(1,:) + X(1,:).*X(1,:);
   AUX_3 = - X(1,:);
   Y11 = -AUX_1; Y12 = - AUX_2;   Y13 =  - AUX_3;
   Y21 =  AUX_1; Y22 =   AUX_2;   Y23 =    AUX_3;
   Y31 =  X(3,:);            Y32 =        0;  Y33 =  X(1,:);
   Y11 = 1-a1 + Y11; Y12 = -1 + Y12; Y13 = 1 + Y13;
   Y21 = a1+ Y21; Y22 = 1 + Y22; Y23 = -1 + Y23;
   Y31 = a1+ Y31;                Y33 = 1 + Y33;
   Y(1,:) = Y11.*X(4,:) + Y12.*X(5,:) + Y13.*X(6,:);
   Y(2,:) = Y21.*X(4,:) + Y22.*X(5,:) + Y23.*X(6,:);
   Y(3,:) = Y31.*X(4,:) + Y32.*X(5,:) + Y33.*X(6,:);
case 5
   a1 = a0 + mue;
   Y = X(1:3,:);
   AUX_1 = 2*a1*X(1,:) + 2*X(2,:) + 2*X(1,:).*X(2,:) - X(3,:);
   AUX_2 = 2*X(1,:) + X(1,:).*X(1,:);
   AUX_3 = - X(1,:);
   Y11 = -AUX_1; Y12 = - AUX_2;   Y13 =  - AUX_3;
   Y21 =  AUX_1; Y22 =   AUX_2;   Y23 =    AUX_3;
   Y31 =  X(3,:);            Y32 =        0;  Y33 =  X(1,:);
   Y11 = 1-a1 + Y11; Y12 = -1 + Y12; Y13 = 1 + Y13;
   Y21 = a1+ Y21; Y22 = 1 + Y22; Y23 = -1 + Y23;
   Y31 = a1+ Y31;                Y33 = 1 + Y33;
   Y(1,:) = Y11.*X(4,:) + Y21.*X(5,:) + Y31.*X(6,:);
   Y(2,:) = Y12.*X(4,:) + Y22.*X(5,:) + Y32.*X(6,:);
   Y(3,:) = Y13.*X(4,:) + Y23.*X(5,:) + Y33.*X(6,:);
end
