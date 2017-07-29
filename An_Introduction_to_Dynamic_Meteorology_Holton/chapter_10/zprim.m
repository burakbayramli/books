function zprim = zprim(t,z,flag,N, lam2,beta, K, H,zf)
% Function to integrate Charney-Devore equations
% DEFINE COEFFICIENTS
zprim = zeros(6,1);
con = 8*sqrt(2)*N/(15.*pi);
gn1 = 5*con;
gn2 = 4*con;
gn3 = 8*con;
an1 = N*N*gn1/(N*N +1+lam2);
an2 = (N*N + 3)*gn2/(N*N +4+lam2);
dn1 = N*N*gn3/(N*N +1+lam2);
dn2 = (N*N-3)*gn3/(N*N +4+lam2);
en = 3*gn3/(4+lam2);
k01 = K/(1.+lam2);
k02 = 4*K/(4 + lam2);
kn1 = (N*N+1)*K/(N*N +1+lam2);
kn2 = (N*N + 4)*K/(N*N +4+lam2);
bn1 = N*beta/(N*N +1+lam2);
bn2 = N*beta/(N*N +4+lam2);
h01 = gn1*H/(1+lam2);
h02 = gn3*H/(4+lam2);
hn1 = gn1*H/(N*N +1+lam2);
hn2 = gn3*H/(N*N +4+lam2);
% EQUATIONS (11)- (16)  from CHARNEY AND DEVORE
%
zprim(1) = -k01*(z(1) - zf(1)) + h01*z(3);
zprim(2) = -(an1*z(1)-bn1)*z(3) - dn1*z(4)*z(6)-kn1*(z(2)-zf(2));
zprim(3) = +(an1*z(1)-bn1)*z(2) + dn1*z(4)*z(5)-kn1*(z(3)-zf(3)) - hn1*z(1);
zprim(4) = en*(z(2)*z(6)-z(3)*z(5))-k02*(z(4)-zf(4)) + h02*z(6);
zprim(5) = - (an2*z(1)-bn2)*z(6) - dn2*z(4)*z(3) - kn2*(z(5) - zf(5));
zprim(6) = + (an2*z(1)-bn2)*z(5) + dn2*z(4)*z(2) - kn2*(z(6) - zf(6))- hn2*z(4);

