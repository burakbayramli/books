function [MMM,MME,INVLUMP,LUMP] = massfun(p,e,t)
eeps = 0.9; % selective lumping parammeter
N = size(p,2); M = size(t,2);
MMM  = sparse(N,N); MME = MMM; LUMP = zeros(N,1);
MM   = [2, 1, 1; 1, 2, 1; 1, 1, 2]; % MM*DET/24 mass matrix
X = p(1,:); Y = p(2,:); 
for I = 1:M
   K   = t(1:3,I);
   X21 = X(K(2)) - X(K(1)); X31 = X(K(3)) - X(K(1));
   X32 = X(K(3)) - X(K(2));
   Y21 = Y(K(2)) - Y(K(1)); Y31 = Y(K(3)) - Y(K(1));
   Y32 = Y(K(3)) - Y(K(2));
   DET = X21*Y31 - X31*Y21;
   DET24 = DET/24;
   MMM(K,K) = MMM(K,K) + MM*DET24;
   LUMP(K)  = LUMP(K) + DET/6; % lumped mass matrix
   MME(K,K) = MME(K,K) + eeps*eye(3)*DET/6 + (1 - eeps)*MM*DET24;
   % following bad 
   % LUMP(K)  = LUMP(K) + DET/12; % lumped mass matrix
   % MME(K,K) = MME(K,K) + eeps*eye(3)*DET/12 + (1 - eeps)*MM*DET24;
end
% diag(MMM) and LUMP/2 are identical,
% also sum(MMM).' and LUMP are (nearly) identical 
AUX = ones(N,1)./LUMP; 
INVLUMP = spdiags(AUX,0,N,N); 
LUMP = spdiags(LUMP,0,N,N);
