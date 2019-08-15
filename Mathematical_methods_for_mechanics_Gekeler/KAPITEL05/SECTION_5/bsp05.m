function Y = bsp05(flag,mue,X,Parmeter);
% Lorentz equation
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
P  = Parmeter(1); b = Parmeter(2); R0 = Parmeter(3);
S  = Parmeter(4); Y0 = Parmeter(5:7);
switch flag
case 1
A  = [P, -P, 0; -1, 1, S; -S, -S, b];
[V,D] = eig(A); omga0 = imag(D(2,2));
c     = V(:,3); c     = sqrt(2)*c/norm(c);
Re_c  = real(c); Im_c  = imag(c); AA = A.';
[V,D] = eig(AA);
d     = V(:,2); d = d/norm(d);
d     = 2*d/conj(d'*c);
Re_d  = real(d); Im_d  = imag(d);
AUX   = [omga0; 0; 0];
B     = [0, 0, 0; -1, 0, 0; 0, 0, 0];
Y     = [A, B, Re_c, Im_c, Re_d, Im_d, AUX];
% Testen der Eigenvektoren
%DIFF1 = A*c + i*omga0*c
%DIFF2 = (A')*d - i*omga0*d
%c_Hc = c'*c; d_Hc = d'*c; d_Tc = norm(d.'*c);
%c_Hc_d_Hc_d_Tc = [c_Hc,d_Hc,d_Tc]
%pause
case 2, Y = X;
   Y(1,:) = zeros(1,size(X,2)); 
   Y(2,:) = X(1,:).*X(3,:); 
   Y(3,:) = - X(1,:).*X(2,:);
case 3, Y = X;
   A  = [P, -P, 0; -1-mue, 1, S; -S, -S, b];
   Y(1,:) = zeros(1,size(X,2));
   Y(2,:) = X(1,:).*X(3,:);
   Y(3,:) = - X(1,:).*X(2,:);
   Y = A*X + Y;
case 4, Y = X(1:3,:);
   Y11 =        0; Y12 =        0; Y13 =      0;
   Y21 =   X(3,:); Y22 =        0; Y23 = X(1,:);
   Y31 = - X(2,:); Y32 = - X(1,:); Y33 =      0;
   Y11 =  P + Y11; Y12 = -P + Y12;
   Y21 = -(1+mue)+ Y21; Y22 =  1 + Y22; Y23 = S + Y23;
   Y31 =      -S + Y31; Y32 = -S + Y32; Y33 = b + Y33;
   Y(1,:) = Y11.*X(4,:) + Y12.*X(5,:) + Y13.*X(6,:);
   Y(2,:) = Y21.*X(4,:) + Y22.*X(5,:) + Y23.*X(6,:);
   Y(3,:) = Y31.*X(4,:) + Y32.*X(5,:) + Y33.*X(6,:);
case 5
   Y = X(1:3,:);
   Y11 =        0; Y21 =        0; Y31 =      0;
   Y12 =   X(3,:); Y22 =        0; Y32 = X(1,:);
   Y13 = - X(2,:); Y23 = - X(1,:); Y33 =      0;
   Y11 =  P + Y11; Y21 = -P + Y21;
   Y12 = -(1+mue)+ Y12; Y22 =  1 + Y22; Y32 = S + Y32;
   Y13 =      -S + Y13; Y23 = -S + Y23; Y33 = b + Y33;
   Y(1,:) = Y11.*X(4,:) + Y12.*X(5,:) + Y13.*X(6,:);
   Y(2,:) = Y21.*X(4,:) + Y22.*X(5,:) + Y23.*X(6,:);
   Y(3,:) = Y31.*X(4,:) + Y32.*X(5,:) + Y33.*X(6,:);
end
