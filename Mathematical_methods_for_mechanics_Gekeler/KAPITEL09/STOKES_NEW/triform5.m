function [CC,DD] = triform5(p,p1,t,t1,U,V,U1,V1)
% Convection term for Taylor-Hood elements
% CC : Matrix CC(U,V) convection term
% DD : Gradient of CC(U,V) for NEWTON method
N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
CC11 = sparse(N,N); CC12 = CC11; CC21 = CC11; CC22 = CC11;
DD11 = sparse(N,N); DD12 = DD11; DD21 = DD11; DD22 = DD11;

for I = 1:size(t,2)
   J = t(1:3,I); K = t1(:,I); L = [J;K];
   XK  = p(1,J); YK  = p(2,J);
   UK  = U(L);   VK  = V(L);
   UK1 = U1(L);  VK1 = V1(L);
   [CCE1,CCE2,CCE3,CCE4,DDE1,DDE2] = triform5_aux(XK,YK,UK,VK,UK1,VK1);
   CC11(L,L) = CC11(L,L) + CCE1;
   CC12(L,L) = CC12(L,L) + CCE2;
   CC21(L,L) = CC21(L,L) + CCE3;
   CC22(L,L) = CC22(L,L) + CCE4;
   DD11(L,L) = DD11(L,L) + DDE1;
   DD22(L,L) = DD22(L,L) + DDE2;
end
CC = [CC11,CC12;CC21,CC22];
DD = [DD11,DD12;DD21,DD22];

