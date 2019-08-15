function Y = bsp02(flag,mue,X,Parmeter);
% Feedback Inhibition Model
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
switch flag
case 1
   A   = [1, 0, 2; -2, 1, 0; 0,-2, 1];
   [V,D] = eig(A); omga0 = imag(D(1,1));
   c     = V(:,2); c     = sqrt(2)*c/norm(c);
   Re_c  = real(c); Im_c  = imag(c);
   Re_d  = Re_c;    Im_d  = Im_c;
   AUX = [omga0; 0; 0];
   B   = [0   0  0.5; -0.5   0   0; 0  -0.5   0];
   Y   = [A, B, Re_c, Im_c, Re_d, Im_d, AUX];
   % Testen der Eigenvektoren
   % c = Re_c + sqrt(-1)*Im_c; d = Re_d + sqrt(-1)*Im_d;
   % DIFF1 = norm(A*c + sqrt(-1)*omga0*c);
   % DIFF2 = norm((A')*d - sqrt(-1)*omga0*d);
   % c_Hc = c'*c; d_Hc = d'*c; d_Tc = norm(d.'*c);
   % DIFF1_DIFF2    = [DIFF1,DIFF2]
   % c_Hc_d_Hc_d_Tc = [c_Hc,d_Hc,d_Tc]
   % pause
case 2
   Y    = X;
   Y(1,:) =   g(mue,X(3,:)) - mue*X(3,:)/2;
   Y(2,:) = - g(mue,X(1,:)) + mue*X(1,:)/2;
   Y(3,:) = - g(mue,X(2,:)) + mue*X(2,:)/2;
case 3
   Y      = X;
   Y(1,:) =    X(1,:) + 2*X(3,:) + g(mue,X(3,:));
   Y(2,:) = -2*X(1,:) +   X(2,:) - g(mue,X(1,:));
   Y(3,:) = -2*X(2,:) +   X(3,:) - g(mue,X(2,:));
case 4
   Y = X(1:3,:);
   Y11 = 1;                Y12 = 0;                Y13 = 2+g1(mue,X(3,:));
   Y21 =-2-g1(mue,X(1,:)); Y22 = 1;                Y23 = 0;
   Y31 = 1;                Y32 =-2-g1(mue,X(2,:)); Y33  = 1;
   Y(1,:) = Y11.*X(4,:) + Y12.*X(5,:) + Y13.*X(6,:);
   Y(2,:) = Y21.*X(4,:) + Y22.*X(5,:) + Y23.*X(6,:);
   Y(3,:) = Y31.*X(4,:) + Y32.*X(5,:) + Y33.*X(6,:);
case 5
   Y   = X(1:3,:);
   Y11 = 1;                Y21 = 0;                Y31 = 2+g1(mue,X(3,:));
   Y12 =-2-g1(mue,X(1,:)); Y22 = 1;                Y32 = 0;
   Y13 = 1;                Y23 =-2-g1(mue,X(2,:)); Y33  = 1;
   Y(1,:) = Y11.*X(4,:) + Y12.*X(5,:) + Y13.*X(6,:);
   Y(2,:) = Y21.*X(4,:) + Y22.*X(5,:) + Y23.*X(6,:);
   Y(3,:) = Y31.*X(4,:) + Y32.*X(5,:) + Y33.*X(6,:);
end

function Y = g(MUE,X)
AUX = (1 + 2*X).^(MUE + 4);
AUX = real(AUX);
Y = 0.5*(AUX - 1)./(AUX + 1) - 2*X;

function Y = g1(MUE,X)  % Ableitung von g
AUX = (1 + 2*X).^(MUE + 3);
AUX = real(AUX);
Y = (2*MUE + 8)*AUX./((1 + 2*X).*AUX + 1).^2 - 2;
