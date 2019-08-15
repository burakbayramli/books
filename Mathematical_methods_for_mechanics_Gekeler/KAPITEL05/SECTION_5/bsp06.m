function Y = bsp06(flag,mue,X,Parmeter);
% Periodisches Hamiltonsches System
% omega*u' + A*u + mue*B*u + f(mue,u) = 0
% here we have mue = 0!!!
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
case 1,
   omga0 = 1;
   AUX   = [omga0; 0];
   A     = [0   -1; 1    0];
   B     = [0    0; 0    0];
   Re_c  = [1; 0];
   Im_c  = [0; 1];
   Re_d  = [1; 0];
   Im_d  = [0; 1];
   Y     = [A, B, Re_c, Im_c, Re_d, Im_d, AUX];
case 2, Y = X;
   Y(1,:) = zeros(1,size(X,2));
   Y(2,:) = X(1,:).*X(1,:);
case 3, Y = X;
   Y(1,:) = - X(2,:);
   Y(2,:) =   X(1,:) + X(1,:).*X(1,:);
case 4, Y = X(1:2,:);
   Y11 = 0;            Y12 = - 1;
   Y21 = 1 + 2*X(1,:); Y22 = 0;
   Y(1,:) = Y11.*X(3,:) + Y12.*X(4,:);
   Y(2,:) = Y21.*X(3,:) + Y22.*X(4,:);
end
if flag == 5
   Y      = X(1:2,:);
   Y11    =   0;
   Y12    =   1 + 2*X(1,:);
   Y21    = - 1;
   Y22    =   0;
   Y(1,:) = Y11.*X(3,:) + Y12.*X(4,:);
   Y(2,:) = Y21.*X(3,:) + Y22.*X(4,:);
end
