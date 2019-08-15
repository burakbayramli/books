function [MMM,MME,LUMP] = massfun(p,e,t)
eeps = 0.9; % selective lumping parammeter
N = size(p,2); M = size(t,2);
MMM  = sparse(N,N); MME = MMM; LUMP = zeros(N,1);
MM   = [2, 1, 1; 1, 2, 1; 1, 1, 2]; % MM/12 mass matrix
X = p(1,:); Y = p(2,:); 
for I = 1:M
   K   = t(1:3,I);
   X21 = X(K(2)) - X(K(1)); X31 = X(K(3)) - X(K(1));
   X32 = X(K(3)) - X(K(2));
   Y21 = Y(K(2)) - Y(K(1)); Y31 = Y(K(3)) - Y(K(1));
   Y32 = Y(K(3)) - Y(K(2));
   DET = X21*Y31 - X31*Y21;
   MMM(K,K) = MMM(K,K) + MM*DET/24;
   LUMP(K) = LUMP(K) + DET/6; % lumped mass matrix
   MME(K,K) = MME(K,K) + eeps*DET*eye(3)/6 + (1 - eeps)*DET*MM/24;
end
MMM = sparse(MMM); MME = sparse(MME);
