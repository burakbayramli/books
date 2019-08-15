function CC = triform3(p,p1,t,t1,UU,S,NU)
% Diffusion and convection term for Taylor-Hood element
N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
CC11 = sparse(N,N); CC12 = CC11; CC21 = CC11; CC22 = CC11;
U = UU(1:N); V = UU(N+1:2*N);

for I = 1:size(t,2)
   J = t(1:3,I); K = t1(:,I); L = [J;K];
   XK = p(1,J); YK = p(2,J); UK = U(L); VK = V(L);
   [A,B,C,D] = triform1_aux(XK,YK,UK,VK);
   CC11(L,L) = CC11(L,L) + A;
   CC12(L,L) = CC12(L,L) + B;
   CC21(L,L) = CC21(L,L) + C;
   CC22(L,L) = CC22(L,L) + D;
end
CC = [CC11 + NU*S, CC12; CC21, CC22 + NU*S];
CC = sparse(CC);