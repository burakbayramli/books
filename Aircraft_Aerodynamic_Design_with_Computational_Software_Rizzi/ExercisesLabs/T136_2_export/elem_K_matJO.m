function [K] = elem_K_matJO(A,E,G,Iy,Iz,Ip,L)
% order wx wy wz Qx Qy Qz
K = zeros(12,12);
EIz = E*Iz;
EIy = E*Iy;
L3  = L^3;
L2  = L^2;
%
EIzL3 = EIz/L3;
EIzL2 = EIz/L2;
EIzL  = EIz/L;
%
EIyL3 = EIy/L3;
EIyL2 = EIy/L2;
EIyL  = EIy/L;
%
EAL  = E*A/L;
GIpL = G*Ip/L;
%
K( 1, 1) =  EAL;
K( 7, 1) = -EAL;

K( 2, 2) =  12*EIzL3;
K( 6, 2) =   6*EIzL2;
K( 8, 2) = -12*EIzL3;
K(12, 2) =   6*EIzL2;

K( 3, 3) = 12*EIyL3;
K( 5, 3) = -6*EIyL2;
K( 9, 3) =-12*EIyL3;
K(11, 3) = -6*EIyL2;

K( 4, 4) = GIpL;
K(10, 4) =-GIpL;

K( 3, 5) =-6*EIyL2;
K( 5, 5) = 4*EIyL;
K( 9, 5) = 6*EIyL2;
K(11, 5) = 2*EIyL;

K( 2, 6)= 6*EIzL2;
K( 6, 6)= 4*EIzL;
K( 8, 6)=-6*EIzL2;
K(12, 6)= 2*EIzL;

K( 1, 7)=-EAL;
K( 7, 7)= EAL;

K( 2, 8)=-12*EIzL3;
K( 6, 8)=- 6*EIzL2;
K( 8, 8)= 12*EIzL3;
K(12, 8)=- 6*EIzL2;

K( 3, 9)=-12*EIyL3;
K( 5, 9)=  6*EIyL2;
K( 9, 9)= 12*EIyL3;
K(11, 9)=  6*EIyL2;

K( 4,10)=-GIpL;
K(10,10)= GIpL;

K( 3,11)=-6*EIyL2;
K( 5,11)= 2*EIyL;
K( 9,11)= 6*EIyL2;
K(11,11)= 4*EIyL;

K( 2,12)= 6*EIzL2;
K( 6,12)= 2*EIzL;
K( 8,12)=-6*EIzL2;
K(12,12)= 4*EIzL;
