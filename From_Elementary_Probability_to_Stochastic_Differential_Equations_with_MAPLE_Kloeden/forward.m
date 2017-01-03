%FORWARD   Forward Time Central Space on Black-Scholes PDE
%          for European call
% 

clf

%%%%%%% Problem and method parameters %%%%%%%
E = 4; sigma = 0.5; r = 0.03; T = 1;
Nx = 11; Nt = 29; L = 10; k = T/Nt; h = L/Nx;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

T1 =  diag(ones(Nx-2,1),1) - diag(ones(Nx-2,1),-1);
T2 = -2*eye(Nx-1,Nx-1) + diag(ones(Nx-2,1),1) + diag(ones(Nx-2,1),-1);
mvec = [1:Nx-1]; D1 = diag(mvec); D2 = diag(mvec.^2);
Aftcs = (1-r*k)*eye(Nx-1,Nx-1) + 0.5*k*sigma^2*D2*T2 + 0.5*k*r*D1*T1;

U = zeros(Nx-1,Nt+1); Uzero = max([h:h:L-h]'-E,0);
U(:,1) = Uzero; p = zeros(Nx-1,1);

for i = 1:Nt
    tau = (i-1)*k;
    p(end) = 0.5*k*(Nx-1)*((sigma^2)*(Nx-1)+r)*(L-E*exp(-r*tau));
    U(:,i+1) = Aftcs*U(:,i) + p;
end

waterfall(U'), xlabel('j'), ylabel('i')

